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
  | Unit deriving (Eq, Show)
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
  deriving (Show)
-- SessionTypes
data SessionType = SAction [(SessionAction, SessionType)]
  | SRecursion RecursionVar SessionType
  | SRecursionVar RecursionVar
  | SDisconnect Role
  | SEnd
  | STypeIdentifier SessionTypeName
  deriving (Show)
-- TypeAlias
data TypeAlias = SessionTypeAlias SessionType SessionType deriving (Show)
-- Actor Definition
data ActorDef = EActorDef Actor SessionType Computation deriving (Show)
-- Protocol
data Protocol = Protocol Role SessionType deriving (Show)
-- Program
type Program = ([TypeAlias], [ActorDef], [Protocol], Computation)




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
type RecEnv = [(String, SessionType)]


data Record = Record { actorDefs :: [ActorDef]
  , protocols :: [Protocol]
  , typeAliases :: [TypeAlias]
  , followST :: SessionType
  , typeEnv :: TypeEnv
  , recEnv :: RecEnv
  , session :: SessionType
} deriving (Show)



data Error = UnboundVariable String
        | UnboundRecursionVariable String
        | RecursionVariableMismatch String Type Type
        | ActorNotDefined String
        | ValueError Type
        | TypeMismatch Type Type
        | SessionMismatch SessionType SessionType
        | BranchError Role Label

instance Show Error where
  show (UnboundVariable var) = "Unbounded variable: " ++ (show var)
  show (UnboundRecursionVariable label) = "Unbounded recursion variable at" ++ (show label)
  show (ActorNotDefined actor) = "Actor " ++ show actor ++ "is not defined"
  show (ValueError value) = "Error at value: " ++ show value
  show (TypeMismatch t1 t2) = "Types: " ++ show t1 ++ " and " ++ show t2 ++ "are not compatible"
  show (SessionMismatch s1 s2) = "Types: " ++ show s1 ++ " and " ++ show s2 ++ "are not compatible"
  -- Change to more specific
  show (BranchError role label) = "SessionAction: " ++ show role ++ show label ++ "does not exists" 
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





-- HELPER FUNCTIONS
returnSessionType :: [ActorDef] -> Actor -> Maybe SessionType
returnSessionType [] _ = Nothing
returnSessionType ((EActorDef a s _):ax) actor =
  if a == actor
    then Just s
  else returnSessionType ax actor

-- let actorDefs = [EActorDef "PingerActor" (SAction [(SConnect "Ponger" "ping" TBool,SAction [(SReceive "Ponger" "pong" TInt,SAction [(SWait "Ponger",SRecursion "browse" (SAction [(SConnect "Pinger" "pong" Unit,SRecursionVar "browse")]))])])]) (EAssign "pid" (EAct (EDiscover (STypeIdentifier "Ponger"))) (ESequence (EAct (EConnect "ping" EUnit (EVar "pid") "Ponger")) (ESequence (EAct (EReceive "Ponger" [("pong",EBool False,EAct (EWait "Ponger")),("ping",EVar "x",EAct (ERaise Unit))])) (ERecursion "browse" (ESequence (EAct (ERaise TInt)) (ESequence (EAct ESelf) (EAct (EContinue "browse")))))))),EActorDef "PongerActor" (SAction [(SAccept "Pinger" "ping" Unit,SAction [(SSend "Pinger" "pong" Unit,SDisconnect "Ponger")])]) (EAct (EAccept "Pinger" [("ping",EBool True,ESequence (EAct (ESend "pong" EUnit "Pinger")) (EAct (EDisconnect "Pinger"))),("pong",EVar "heyy",EAct (ERaise (EPid (STypeIdentifier "Session"))))])),EActorDef "PongerActor" (STypeIdentifier "Ponger") (EAct (EAccept "Pinger" [("ping",EUnit,ESequence (EAct (ESend "pong" EUnit "Pinger")) (EAct (EDisconnect "Pinger"))),("pong",EInt 1234,EAct (ERaise TString))]))]


checkBehaviour :: Record -> Behaviour -> TypeCheck (Type, SessionType)
checkBehaviour record (EStop) = do
  return (Unit, (session record))

checkBehaviour record (EComp comp) = do
  (ty, session) <- checkComputation record comp
  return (ty, session)





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
  let sessionU = returnSessionType (actorDefs record) actor
  case sessionU of
    Just s -> return (EPid s, (session record))
    Nothing -> throwError (ActorNotDefined actor)

-- T-SELF
checkComputation record (EAct (ESelf)) = do
  return (EPid (followST record), (session record))

-- T-DISCOVER
checkComputation record (EAct (EDiscover s)) = do
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
checkComputation record (EAct (ERaise ty)) = do
  return (ty, (session record))

-- T-TRY
checkComputation record (ETry action comp) = do
  (ty1, session1) <- checkComputation record (EAct (action))
  (ty2, session2) <- checkComputation record comp
  -- if ty1 /= ty2 then throwError (TypeMismatch ty1 ty2)
  -- else if session1 /= session2 then throwError (SessionMismatch session1 session2)
  -- else return (ty1, session1)
  return (ty1, session1)


-- SESSION COMMUNICATION RULES
-- T-CONN
{- STEPS : connect label(value1) to value2 as role
   1. Check if <role> !! label (type) exists in session
   2. checkValue value1, should match types with type, V:A
   3. checkValue value2, should match Pid(followST), W:Pid(T)
   4. T = sessionType of role
   5. return 1 and corresponding updated sessionType
-}


-- checkAction :: SessionType -> Role -> Label -> Maybe Type
-- checkAction (SAction []) role label = Nothing
-- checkAction (SAction [x:xs]) role label = case x of
--                                             (SConnect role' label' typeA) -> if role == role' && label == label'
--                                                                            then typeA else checkAction xs role label
-- checkAction _ role label = Nothing



-- checkComputation record ((EConnect label valueV valueW role)) = do
--   (action, session') <- extractAction (session record)
--   typeV <- checkValue (typeEnv record) valueV
--   typeW <- checkValue (typeEnv record) valueW
--   sessionType' <- getSessionType (protocols record) role
--   returnType <- checkAction (session record) role label

--   case returnType of
--     Just typeA -> if typeV /= typeA
--                     then throwError(TypeMismatch typeV typeA)
--                   else if typeW /= (EPid followST)
--                     then throwError (ValueError valueW)
--                   else if followST /= sessionType'
--                     then throwError (SessionMismatch followST sessionType')  -- Change to more specific
--                   else return (Unit, session')
--     Nothing    -> throwError (BranchError role label)

-- T-SEND
{- STEPS : send label(value) to role
   1. Check if <role> ! label (type) exists in single-sessions
   2. checkValue value, match types with A
   3. return 1 and corresponding updated sessionType
-}

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

-- T-WAIT
{- STEPS : #↑ q . S ▷ wait q :1 ◁ S
   1. Find SessionAction SWait role in session.
   2. Return unit type and rest of sessionType
-}

-- T-DISCONN
{- STEPS : #↓ q ▷ disconnect from q :1 ◁ end
   1. Find SessionAction SDisconnect role in session.
   2. Return unit type and end
-}