import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Text (Text)
import Data.Void
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Map as Map

type Label = String
type Binder = String
type Choices = [(Label, EValue, Computation)]
type RecursionVar = String
type SessionTypeName = String
-- Actor Name
type Actor = String
-- Roles
type Role = String
-- Behaviours
data Behaviour = EComp Computation | EStop deriving (Show)
-- Types
data Type = EPid SessionType
  | TString
  | TInt
  | TBool
  | Unit
  deriving (Eq, Show)
-- Values
data EValue = EVar String
  | EInt Int
  | EBool Bool
  | EUnit
  deriving (Eq, Ord, Show)
-- Actions
data EAction = EReturn EValue
  | EContinue Label
  | ERaise Type
  | ENew Actor
  | ESelf
  | EReplace EValue Behaviour
  | EDiscover SessionType
  | EConnect Label EValue EValue Role
  | EAccept Role Choices
  | ESend Label EValue Role
  | EReceive Role Choices
  | EWait Role
  | EDisconnect Role
  deriving (Show)
-- Computations
data Computation = EAssign Binder Computation Computation
  | ETry EAction Computation
  | ERecursion Label Computation
  | EAct EAction
  | ESequence Computation Computation
  deriving (Show)
-- Session Actions
data SessionAction = SSend Role Label Type
  | SConnect Role Label Type
  | SReceive Role Label Type
  | SAccept Role Label Type
  | SWait Role
  deriving (Eq, Show)
-- SessionTypes
data SessionType = SAction [(SessionAction, SessionType)]
  | SRecursion RecursionVar SessionType
  | SRecursionVar RecursionVar
  | SDisconnect Role
  | SEnd
  | STypeIdentifier SessionTypeName
  deriving (Eq, Show)
-- TypeAlias
data TypeAlias = SessionTypeAlias SessionType SessionType deriving (Show)
-- Actor Definition
data ActorDef = EActorDef Actor SessionType Computation deriving (Show)
-- Protocol
data Protocol = Protocol Role SessionType deriving (Show)
-- Program
type Program = ([TypeAlias], [ActorDef], [Protocol], Computation)


{-
compareTypes :: Type -> Type -> TC Type
compareTypes Any t = return t
compareTypes t Any = return t
compareTypes t1 t2 = if t1 <> t2 then throwError (IncompatibleTypes t1 t2) else return t1


https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
http://www.dcs.gla.ac.uk/~ornela/projects/Artem%20Usov.pdf

-}

type TypeEnv = [(EValue, Type)]
type RecEnv = [(String, SessionType)]


data Record = Record { actorDefs :: [ActorDef]
  , protocols :: [Protocol]
  , typeAliases :: [TypeAlias]
  , followST :: SessionType
  , typeEnv :: TypeEnv
  , recEnv :: RecEnv
  , session :: SessionType
}



data Error = UnboundVariable EValue
        | UnboundRecursionVariable String
        | RecursionVariableMismatch String Type Type
        | ActorNotDefined String
        | ValueError Type
        | TypeMismatch Type Type
        | SessionMismatch SessionType SessionType
        | BranchError Role Label
        | BranchRoleError Role
        | SessionConsumptionError SessionType
        | UndefinedProtocol Role
        | RoleMismatch Role

instance Show Error where
  show (UnboundVariable val) = "Unbounded variable: " ++ (show val)
  show (UnboundRecursionVariable label) = "Unbounded recursion variable at" ++ (show label)
  show (ActorNotDefined actor) = "Actor " ++ show actor ++ "is not defined"
  show (ValueError value) = "Error at value: " ++ show value
  show (TypeMismatch t1 t2) = "Types: " ++ show t1 ++ " and " ++ show t2 ++ " are not compatible"
  show (SessionMismatch s1 s2) = "Mismatch of SessionTypes: " ++ show s1 ++ " and " ++ show s2
  show (BranchError role label) = "SessionAction: " ++ show role ++ " " ++ show label ++ " does not exists"
  show (BranchRoleError role) = "Role " ++ show role ++ " not found"
  show (SessionConsumptionError session) = "Incomplete consumption of session: " ++ show session
  show (UndefinedProtocol role) = "Role " ++ show role ++ "has no defined protocol"
  show (RoleMismatch role) = "Mismatched Role " ++ show role

type TypeCheck = Either Error


-- HELPER FUNCTIONS

-- Returns SessionType of input Actor from list of ActorDefs
returnSessionType :: [ActorDef] -> Actor -> TypeCheck SessionType
returnSessionType [] actor = throwError (ActorNotDefined actor)
returnSessionType ((EActorDef a s _):ax) actor =
  if a == actor
    then return s
  else returnSessionType ax actor


-- Returns SessionType of input Role from list of Protocols
getSessionType :: [Protocol] -> Role -> TypeCheck SessionType
getSessionType [] role = throwError (UndefinedProtocol role)
getSessionType ((Protocol role' session'):xs) role = do
  if role == role'
    then return session'
  else getSessionType xs role


-- Returns (Type, SessionType) of matching Role, Label in CONNECT SessionAction
getConnectAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getConnectAction (SAction []) role label = throwError (BranchError role label)
getConnectAction (SAction (x:xs)) role label = do
  case x of
    ((SConnect role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getConnectAction (SAction xs) role label
    _  -> getConnectAction (SAction xs) role label
getConnectAction _ role label = throwError (BranchError role label)


-- Returns (Type, SessionType) of matching Role, Label in SEND SessionAction
getSendAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getSendAction (SAction []) role label = throwError (BranchError role label)
getSendAction (SAction (x:xs)) role label = do
  case x of
    ((SSend role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getSendAction (SAction xs) role label
    _  -> getSendAction (SAction xs) role label
getSendAction _ role label = throwError (BranchError role label)


-- Returns (Type, SessionType) of matching Role, Label in WAIT SessionAction
getWaitAction :: SessionType -> Role -> TypeCheck (Type, SessionType)
getWaitAction (SAction []) role = throwError (BranchRoleError role)
getWaitAction (SAction (x:xs)) role = do
  case x of
    ((SWait role'),session) ->
      if role == role' then return (Unit, session)
      else getWaitAction (SAction xs) role
    _ -> getWaitAction (SAction xs) role
getWaitAction _ role = throwError (BranchRoleError role)


-- Returns (Type, SessionType) of matching Role, Label in ACCEPT SessionAction
getAcceptAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getAcceptAction (SAction []) role label = throwError (BranchError role label)
getAcceptAction (SAction (x:xs)) role label = do
  case x of
    ((SAccept role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getAcceptAction (SAction xs) role label
    _  -> getAcceptAction (SAction xs) role label
getAcceptAction _ role label = throwError (BranchError role label)



-- Returns list of (Type, SessionType) of all the branches/choices in ACCEPT SessionAction
-- getAcceptBranches :: Record -> Role -> Choices -> [(Type, SessionType)]
getAcceptBranches record role choices = do
  actionsList <- map (\(label, value, computation) ->
    (value, fromRight (TString, SEnd) (getAcceptAction (session record) role label), computation)) choices
  results <- map (\(value, (tyI, sessionI), computation) ->
    fromRight (TString, SEnd) (checkComputation record{typeEnv=(value,tyI):(typeEnv record), session=sessionI} computation)) [actionsList]
  return results


-- Checks if all the (Type, SessionType) are same and returns it
checkAllEqual :: [(Type, SessionType)] -> TypeCheck (Type, SessionType)
checkAllEqual xs = do
  let checkList = map (== head xs) (tail xs)
  if and checkList == True
    then return $ head xs
  else do
    let (ty, session)  = head xs
    let (ty',session') = (tail xs) !! maybe 0 id (elemIndex False checkList)
    if ty /= ty' then throwError (TypeMismatch ty ty')
    else throwError (SessionMismatch session session')



checkBehaviour :: Record -> Behaviour -> TypeCheck ()
-- T-STOP
checkBehaviour record (EStop) = return ()

checkBehaviour record (EComp comp) = do
  (ty, session') <- checkComputation record comp
  if session' == SEnd
    then return ()
  else throwError (SessionConsumptionError (session record))




-- VALUE TYPING
checkValue :: TypeEnv -> EValue -> TypeCheck Type
-- T-UNIT
checkValue typeEnv EUnit = return(Unit)

-- T-VAR
checkValue typeEnv value = do
  let returnType = Map.lookup value $ Map.fromList typeEnv in
    case returnType of
      Just t -> return t
      Nothing -> throwError (UnboundVariable value)



-- T-BODY
checkComputation :: Record -> Computation -> TypeCheck (Type, SessionType)
-- FUNCTIONAL RULES
-- T-LET
checkComputation record (EAssign binder c1 c2) = do
  (ty, session') <- checkComputation record c1
  (ty', session'') <- checkComputation (record { typeEnv=((EVar binder),ty):(typeEnv record), session=session'}) c2
  return (ty', session'')

-- T-REC
checkComputation record (ERecursion label comp) = do
  (ty, session') <- checkComputation record {recEnv=((label,(session record)):(recEnv record))} comp
  return (ty, session')

-- T-CONTINUE
checkComputation record (EAct (EContinue label)) = do
  let returnType = Map.lookup label $ Map.fromList (recEnv record)
  case returnType of
    Just t -> return (TString, t)
    Nothing -> throwError (UnboundRecursionVariable label)

-- T-RETURN
checkComputation record (EAct (EReturn value)) = do
  typeV <- checkValue (typeEnv record) value
  return (typeV, (session record))



-- ACTOR / ADAPTATION RULES
-- T-NEW
checkComputation record (EAct (ENew actor)) = do
  session' <- returnSessionType (actorDefs record) actor
  return (EPid session', (session record))

-- T-SELF
checkComputation record (EAct (ESelf)) = return (EPid (followST record), (session record))

-- T-DISCOVER
checkComputation record (EAct (EDiscover s)) = return (EPid s, (session record))

-- T-REPLACE
checkComputation record (EAct (EReplace value behaviour)) = do
  typeV <- checkValue (typeEnv record) value
  case typeV of
    (EPid sessionU) -> checkBehaviour (record {followST = sessionU}) behaviour
    _               -> throwError (ValueError typeV)
  return (Unit, (session record))

-- EXCEPTION HANDLING
-- T-RAISE
checkComputation record (EAct (ERaise ty)) = return (ty, (session record))

-- T-TRY
checkComputation record (ETry action comp) = do
  (ty1, session1) <- checkComputation record (EAct (action))
  (ty2, session2) <- checkComputation record comp
  if ty1 /= ty2 then throwError (TypeMismatch ty1 ty2)
  else if session1 /= session2 then throwError (SessionMismatch session1 session2)
  else return (ty1, session1)

-- SESSION COMMUNICATION RULES
-- T-CONN
checkComputation record (EAct (EConnect label valueV valueW role)) = do
  (typeA, session') <- getConnectAction (session record) role label
  typeV <- checkValue (typeEnv record) valueV
  typeW <- checkValue (typeEnv record) valueW
  sessionType' <- getSessionType (protocols record) role

  if typeV /= typeA then throwError(TypeMismatch typeV typeA)
  else if typeW /= (EPid (followST record)) then throwError (ValueError typeW)
  else if (followST record) /= sessionType' then throwError (SessionMismatch (followST record) sessionType')
  else return (Unit, session')

-- T-SEND
checkComputation record (EAct (ESend label value role)) = do
  (typeA, session') <- getSendAction (session record) role label
  typeV <- checkValue (typeEnv record) value

  if typeV /= typeA then throwError(TypeMismatch typeV typeA)
  else return (Unit, session')


-- T-ACCEPT
-- checkComputation record (EAct (EAccept role choices)) = do
--   branches <- (getAcceptBranches record role choices)
--   return $ checkAllEqual [branches]

-- T-RECV



-- T-WAIT
checkComputation record (EAct (EWait role)) = do
  (ty, session) <- getWaitAction (session record) role
  return (ty, session)

-- T-DISCONN
checkComputation record (EAct (EDisconnect role)) = do
  case (session record) of
    (SDisconnect role') ->
      if role == role' then return (Unit, SEnd)
      else throwError (RoleMismatch role)
    _ -> throwError (SessionConsumptionError (session record))



-- T-SEQUENCE
checkComputation record (ESequence comp1 comp2) = do
  (ty, session)   <- checkComputation record comp1
  (ty', session') <- checkComputation record{session=session} comp2
  return (ty', session')



-- T-DEF
checkActorDef :: Record -> ActorDef -> TypeCheck ()
checkActorDef record (EActorDef actor session computation) = do
  (ty, session') <- checkComputation record{followST = session, session=session} computation
  if session' == SEnd then return ()
  else throwError (SessionConsumptionError session)



-- T-PROGRAM
checkProgram :: Program -> TypeCheck ()
checkProgram (typeAliases, actorDefs, protocols, computation) = do
  let record = Record { actorDefs=actorDefs
                      , typeAliases=typeAliases
                      , protocols=protocols
                      , typeEnv=[], recEnv=[]}
  mapM_ (checkActorDef record) actorDefs
  return ()





main = do
  -- let s = SAction [ (SConnect "role" "label" TString, SDisconnect "role")
  --                 , (SSend "role" "label" TString, SAction [(SWait "role", SEnd)])
  --                 , (SWait "role", SEnd)
  --                 , (SAccept "role" "label" TString, SEnd)]

  -- let r = Record { typeEnv = [(EVar "v", TString), (EVar "w", EPid (STypeIdentifier "Session"))]
  --                , protocols = [Protocol "role" (STypeIdentifier "HELLO")]
  --                , session = s
  --                , followST = STypeIdentifier "Session" }
  -- -- let result = checkComputation r (EAct (EConnect "label" (EVar "v") (EVar "w") "role"))
  -- let choices = [("label", (EVar "v"), (ESequence (EAct (ESelf)) (EAct (ESelf))) ), ("label", (EVar "v"), (EAct (EDiscover (STypeIdentifier "Hello"))))]
  -- let x = getAcceptBranches r "role" choices
  let r = Record {}
  let x = checkActorDef r (EActorDef "actor" (STypeIdentifier "Session") ((EAct (ESelf))))
  return x