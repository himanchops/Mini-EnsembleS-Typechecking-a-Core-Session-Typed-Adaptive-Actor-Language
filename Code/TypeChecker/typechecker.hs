import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Text (Text)
import Data.Void
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
  deriving (Show)
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



-- instance Eq Type where
--   EPid s1 == EPid s2  = 
--   TString == TString  = True
--   TInt == TInt        = True
--   TBool == TBool      = True
--   Unit == Unit        = True
--   _ == _              = False



{-

Check Program :: Program → TC ()
Check Definition :: Definition → TC()
Check Computation :: SessionType → TypeEnv → SessionType → Computation → TC(Type, SessionType)
Check Value :: TypeEnv → Value → TC Type
-}


{-
compareTypes :: Type -> Type -> TC Type
compareTypes Any t = return t
compareTypes t Any = return t
compareTypes t1 t2 = if t1 <> t2 then throwError (IncompatibleTypes t1 t2) else return t1


https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
http://www.dcs.gla.ac.uk/~ornela/projects/Artem%20Usov.pdf
https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html

-}

type TypeEnv = [(String, Type)]
-- type TypeEnv = [(EValue, Type)]
type RecEnv = [(String, SessionType)]


data Record = Record { actorDefs :: [ActorDef]
  , protocols :: [Protocol]
  , typeAliases :: [TypeAlias]
  , followST :: SessionType
  , typeEnv :: TypeEnv
  , recEnv :: RecEnv
  , session :: SessionType
}



data Error = UnboundVariable String
-- data Error = UnboundVariable EValue
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
  show (UnboundVariable var) = "Unbounded variable: " ++ (show var)
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






-- VALUE TYPING
checkValue :: TypeEnv -> EValue -> TypeCheck Type
-- T-UNIT
checkValue typeEnv EUnit = return(Unit)

-- T-VAR
checkValue typeEnv (EVar value) =
  let returnType = Map.lookup value $ Map.fromList typeEnv in
  case returnType of
    Just t -> return t
    Nothing -> throwError (UnboundVariable value)

checkValue typeEnv (EInt int) =
  let returnType = Map.lookup (show int) $ Map.fromList typeEnv in
  case returnType of
    Just t -> return t
    Nothing -> throwError (UnboundVariable $ show int)

checkValue typeEnv (EBool bool) =
  let returnType = Map.lookup (show bool) $ Map.fromList typeEnv in
  case returnType of
    Just t -> return t
    Nothing -> throwError (UnboundVariable $ show bool)

-- checkValue typeEnv value = do
--   let returnType = Map.lookup value $ Map.fromList typeEnv in
--     case returnType of
--       Just t -> return t
--       Nothing -> throwError (UnboundVariable value)





-- HELPER FUNCTIONS


returnSessionType :: [ActorDef] -> Actor -> TypeCheck SessionType
returnSessionType [] actor = throwError (ActorNotDefined actor)
returnSessionType ((EActorDef a s _):ax) actor =
  if a == actor
    then return s
  else returnSessionType ax actor

getSessionType :: [Protocol] -> Role -> TypeCheck SessionType
getSessionType [] role = throwError (UndefinedProtocol role)
getSessionType ((Protocol role' session'):xs) role = do
  if role == role'
    then return session'
  else getSessionType xs role


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


getWaitAction :: SessionType -> Role -> TypeCheck (Type, SessionType)
getWaitAction (SAction []) role = throwError (BranchRoleError role)
getWaitAction (SAction (x:xs)) role = do
  case x of
    ((SWait role'),session) ->
      if role == role' then return (Unit, session)
      else getWaitAction (SAction xs) role
    _ -> getWaitAction (SAction xs) role
getWaitAction _ role = throwError (BranchRoleError role)


-- getAcceptAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
-- getAcceptAction (SAction []) role label = throwError (BranchError role label)
-- getAcceptAction (SAction (x:xs)) role label = do
--   case x of
--     ((SAccept role' label' typeA),session) ->
--       if role == role' && label == label'
--         then return (typeA,session)
--       else getAcceptAction (SAction xs) role label
--     _  -> getAcceptAction (SAction xs) role label
-- getAcceptAction _ role label = throwError (BranchError role label)



-- getAcceptBranches :: Record -> Role -> Choices -> TypeCheck([Type,SessionType])
-- getAcceptBranches record role choices = do
--   --choices = [(Label, EValue, Computation)]
--   results = 
--     map (\(label, value, computation) -> 
--       (value, getAcceptAction (session record) role label, computation)) choices
--   -- results = [(value, (type, session), computation)]
--   checkComputation record {typeEnv=(value:)}








checkBehaviour :: Record -> Behaviour -> TypeCheck ()
-- T-STOP
checkBehaviour record (EStop) = return ()

checkBehaviour record (EComp comp) = do
  (ty, session') <- checkComputation record comp
  if session' == SEnd
    then return ()
  else throwError (SessionConsumptionError (session record))





checkComputation :: Record -> Computation -> TypeCheck (Type, SessionType)
-- FUNCTIONAL RULES
-- T-LET
checkComputation record (EAssign binder c1 c2) = do
  (ty, session') <- checkComputation record c1
  (ty', session'') <- checkComputation (record { typeEnv=(binder,ty):(typeEnv record), session=session'}) c2
  return (ty', session'')

-- T-REC
checkComputation record (ERecursion label comp) = do
  (ty, session') <- checkComputation record {recEnv=((label,(session record)):(recEnv record))  } comp
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
--   branches <- getAcceptBranches record role choices




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

-- T-ACCEPT
{- STEPS : accept from role (labeli(valuei) -> compi)
   1. For all choices:
   		(ty,ses) <- typeEnv + value:type checkComputation compi
   2. All branches must have same return type and post-condition session
   3. return (ty, ses)
-}

-- T-RECV
{- STEPS : receive from role (labeli(valuei) -> compi)
   1. For all choices:
   		(ty,ses) <- typeEnv + value:type checkComputation compi
   2. All branches must have same return type and post-condition session
   3. return (ty, ses)
-}

main = do
  let s = SAction [ (SConnect "role" "label" TString, SDisconnect "role")
                  , (SSend "role" "label" TString, SAction [(SWait "role", SEnd)])
                  , (SWait "role", SEnd)]

  let r = Record { typeEnv = [("v", TString), ("w", TBool)]
                 , protocols = [Protocol "role" (STypeIdentifier "HELLO")]
                 , session = s }
  -- let result = checkComputation r (EAct (EConnect "label" (EVar "v") (EVar "w") "role"))
  let result = checkComputation r (EAct (EWait "role")) 
  return result