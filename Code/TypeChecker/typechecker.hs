import Control.Applicative
import Control.Monad
import Control.Monad.Except
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
  | TAny
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
  | SAny
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
        | IncompatibleTypes Type Type
        | SessionMismatch SessionType SessionType
        | BranchError Role Label
        | BranchRoleError Role
        | SessionConsumptionError SessionType
        | UndefinedProtocol Role
        | RoleMismatch Role
        | UndefinedAlias SessionType
        | ExpectedPID Type

instance Show Error where
  show (UnboundVariable val) = "Unbounded variable: " ++ (show val)
  show (UnboundRecursionVariable label) = "Unbounded recursion variable at" ++ (show label)
  show (ActorNotDefined actor) = "Actor " ++ show actor ++ "is not defined"
  show (ValueError value) = "Error at value: " ++ show value
  show (IncompatibleTypes t1 t2) = "Types: " ++ show t1 ++ " and " ++ show t2 ++ " are not compatible"
  show (SessionMismatch s1 s2) = "Mismatch of SessionTypes: " ++ show s1 ++ " and " ++ show s2
  show (BranchError role label) = "SessionAction: " ++ show role ++ " " ++ show label ++ " does not exists"
  show (BranchRoleError role) = "Role " ++ show role ++ " not found"
  show (SessionConsumptionError session) = "Incomplete consumption of session: " ++ show session
  show (UndefinedProtocol role) = "Role " ++ show role ++ "has no defined protocol"
  show (RoleMismatch role) = "Mismatched Role " ++ show role
  show (UndefinedAlias session) = "Session " ++ show session ++ " has no defined type alias"
  show (ExpectedPID ty) = "Type error for type " ++ show ty ++ ". Expected PID"

type TypeCheck = Either Error


-- HELPER FUNCTIONS

-- Compare two Types and returns 
compareTypes :: Type -> Type -> TypeCheck Type
compareTypes TAny t = return t
compareTypes t TAny = return t
compareTypes t1 t2 = if t1 /= t2 then throwError (IncompatibleTypes t1 t2) else return t1


compareSessions :: [TypeAlias] -> SessionType -> SessionType -> TypeCheck SessionType
compareSessions tA SAny s = return s
compareSessions tA s SAny = return s
compareSessions tA s1 s2 = getAlias tA s1 >>= \s1' ->
  getAlias tA s2 >>= \s2' ->
  if s1' /= s2' then throwError (SessionMismatch s1 s2) else return s1



-- Returns SessionType of input Actor from list of ActorDefs
getSessionTypeOfActor :: [ActorDef] -> Actor -> TypeCheck SessionType
getSessionTypeOfActor [] actor = throwError (ActorNotDefined actor)
getSessionTypeOfActor ((EActorDef a s _):ax) actor =
  if a == actor
    then return s
  else getSessionTypeOfActor ax actor


-- Returns SessionType of input Role from list of Protocols
getSessionTypeOfRole :: [Protocol] -> Role -> TypeCheck SessionType
getSessionTypeOfRole [] role = throwError (UndefinedProtocol role)
getSessionTypeOfRole ((Protocol role' session'):xs) role = do
  if role == role'
    then return session'
  else getSessionTypeOfRole xs role



-- Returns SessionType for input SessionAlias
getAlias :: [TypeAlias] -> SessionType -> TypeCheck SessionType
getAlias [] (STypeIdentifier session) = throwError (UndefinedAlias (STypeIdentifier session))
getAlias ((SessionTypeAlias sessionName sessionType):xs) (STypeIdentifier session) = do
  if sessionName == (STypeIdentifier session)
    then return sessionType
  else getAlias xs (STypeIdentifier session)
getAlias _ session = return session



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



getReceiveAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getReceiveAction (SAction []) role label = throwError (BranchError role label)
getReceiveAction (SAction (x:xs)) role label = do
  case x of
    ((SReceive role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getAcceptAction (SAction xs) role label
    _  -> getAcceptAction (SAction xs) role label
getReceiveAction _ role label = throwError (BranchError role label)



-- Returns list of (Type, SessionType) of all the branches/choices in ACCEPT SessionAction
getAcceptBranches :: Record -> Role -> Choices -> TypeCheck [(Type, SessionType)]
getAcceptBranches record role choices = do
  actionsList <- mapM (\(label, value, computation) ->
    getAcceptAction (session record) role label >>= \act ->
    return (value, act, computation)) choices
  results <- mapM (\(value, (tyI, sessionI), computation) ->
    checkComputation record{typeEnv=(value,tyI):(typeEnv record), session=sessionI} computation)
    actionsList
  return results


-- Returns list of (Type, SessionType) of all the branches/choices in RECEIVE SessionAction
getReceiveBranches :: Record -> Role -> Choices -> TypeCheck [(Type, SessionType)]
getReceiveBranches record role choices = do
  actionsList <- mapM (\(label, value, computation) ->
    getReceiveAction (session record) role label >>= \act ->
    return (value, act, computation)) choices
  results <- mapM (\(value, (tyI, sessionI), computation) ->
    checkComputation record{typeEnv=(value,tyI):(typeEnv record), session=sessionI} computation)
    actionsList
  return results


-- Checks if all the (Type, SessionType) are same and returns tuple
checkAllEqual :: [(Type, SessionType)] -> TypeCheck (Type, SessionType)
checkAllEqual xs = do
  let checkList = map (== head xs) (tail xs)
  if and checkList == True
    then return $ head xs
  else do
    let (ty, session)  = head xs
    let (ty',session') = (tail xs) !! maybe 0 id (elemIndex False checkList)
    if ty /= ty' then throwError (IncompatibleTypes ty ty')
    else throwError (SessionMismatch session session')



unwrapPID :: Type -> TypeCheck SessionType
unwrapPID (EPid session) = return session
unwrapPID t = throwError (ExpectedPID t)



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
  (ty', session'') <- checkComputation record {typeEnv=((EVar binder),ty):(typeEnv record), session=session'} c2
  return (ty', session'')

-- T-REC
checkComputation record (ERecursion label comp) = do
  (ty, session') <- checkComputation record {recEnv=((label,(session record)):(recEnv record))} comp
  return (ty, session')

-- T-CONTINUE
checkComputation record (EAct (EContinue label)) = do
  let returnType = Map.lookup label $ Map.fromList (recEnv record)
  case returnType of
    Just t -> return (TAny, SAny)
    Nothing -> throwError (UnboundRecursionVariable label)

-- T-RETURN
checkComputation record (EAct (EReturn value)) = do
  typeV <- checkValue (typeEnv record) value
  return (typeV, (session record))



-- ACTOR / ADAPTATION RULES
-- T-NEW
checkComputation record (EAct (ENew actor)) = do
  session' <- getSessionTypeOfActor (actorDefs record) actor
  return (EPid session', (session record))

-- T-SELF
checkComputation record (EAct (ESelf)) = return (EPid (followST record), (session record))

-- T-DISCOVER
checkComputation record (EAct (EDiscover s)) = getAlias (typeAliases record) s >>= \s ->
  return (EPid s, (session record))

-- T-REPLACE
checkComputation record (EAct (EReplace value behaviour)) = do
  typeV <- checkValue (typeEnv record) value
  case typeV of
    (EPid sessionU) -> checkBehaviour (record {followST = sessionU}) behaviour
    _               -> throwError (ValueError typeV)
  return (Unit, (session record))

-- EXCEPTION HANDLING
-- T-RAISE
checkComputation record (EAct (ERaise ty)) = return (ty, SAny)

-- T-TRY
checkComputation record (ETry action comp) = do
  (ty1, session1) <- checkComputation record (EAct (action))
  (ty2, session2) <- checkComputation record comp
  ty <- compareTypes ty1 ty2
  session <- compareSessions (typeAliases record) session1 session2
  return (ty,session)

-- SESSION COMMUNICATION RULES
-- T-CONN
checkComputation record (EAct (EConnect label valueV valueW role)) = do
  (typeA, session') <- getConnectAction (session record) role label
  typeV <- checkValue (typeEnv record) valueV
  typeW <- checkValue (typeEnv record) valueW
  sessionType' <- getSessionTypeOfRole (protocols record) role

  compareTypes typeV typeA
  pidST <- unwrapPID typeW
  compareSessions pidST sessionType'
  -- compareTypes typeW (EPid (followST record))

  compareSessions (typeAliases record) (followST record) sessionType'
  return (Unit, session')


-- T-SEND
checkComputation record (EAct (ESend label value role)) = do
  (typeA, session') <- getSendAction (session record) role label
  typeV <- checkValue (typeEnv record) value

  compareTypes typeV typeA
  return (Unit, session')


-- T-ACCEPT
checkComputation record (EAct (EAccept role choices)) = do
  branches <- getAcceptBranches record role choices
  (ty, session) <- checkAllEqual branches
  return (ty, session)

-- T-RECV
checkComputation record (EAct (EReceive role choices)) = do
  branches <- getReceiveBranches record role choices
  (ty, session) <- checkAllEqual branches
  return (ty, session)


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
  (ty, session') <- getAlias (typeAliases record) session >>= \s -> 
    checkComputation record{followST=s, session=s} computation
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
  let s = SAction [ (SConnect "role" "label" TString, SDisconnect "role")
                  , (SSend "role" "label" TString, SAction [(SWait "role", SEnd)])
                  , (SWait "role", SEnd)
                  , (SAccept "role" "label" TString, SEnd)]

  let r = Record { typeEnv = [(EVar "v", TString), (EVar "w", EPid (STypeIdentifier "Session"))]
                 , protocols = [Protocol "role" (STypeIdentifier "Session")]
                 , session = s
                 , followST = STypeIdentifier "Session"
                 , typeAliases = [] }
  let choices = [("label", (EVar "x"), (ESequence (EAct (ESelf)) (EAct (ESelf))) )
                ,("label", (EVar "v"), (EAct (EDiscover (STypeIdentifier "Session"))))]
  -- let result = checkComputation r (EAct (EConnect "label" (EVar "v") (EVar "w") "role"))
  let result = checkComputation r (EAct (EAccept "role" choices))

  return result