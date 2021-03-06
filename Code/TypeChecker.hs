module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Void
import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Map as Map

import AST

type TypeEnv = [(String, Type)]
type RecEnv = [(Label, SessionType)]


data State = State { actorDefs :: [ActorDef]
  , protocols :: [Protocol]
  , typeAliases :: [TypeAlias]
  , followST :: SessionType
  , typeEnv :: TypeEnv
  , recEnv :: RecEnv
  , session :: SessionType
}



data Error = UnboundVariable String
        | UnboundRecursionVariable Label
        | ActorNotDefined Actor
        | IncompatibleTypes Type Type
        | SessionMismatch SessionType SessionType
        | BranchError Role Label String
        | BranchRoleError Role
        | SessionConsumptionError SessionType
        | UndefinedProtocol Role
        | RoleMismatch Role
        | UndefinedAlias SessionType
        | ExpectedPID Type

instance Show Error where
  show (UnboundVariable val) = "Unbounded variable: " ++ (show val)
  show (UnboundRecursionVariable label) = "Unbounded recursion variable at" ++ (show label)
  show (ActorNotDefined actor) = "Actor " ++ show actor ++ " is not defined"
  show (IncompatibleTypes t1 t2) = "Types: " ++ show t1 ++ " and " ++ show t2 ++ " are not compatible"
  show (SessionMismatch s1 s2) = "Mismatch of SessionTypes: " ++ show s1 ++ " and " ++ show s2
  show (BranchError role label action) = show action ++ " action for role " ++ show role ++ " and label " ++ show label ++ " does not exist" 
  show (BranchRoleError role) = "Action wait " ++ role ++ " not found"
  show (SessionConsumptionError (STypeIdentifier session)) = "Incomplete consumption of session: " ++ show session
  show (SessionConsumptionError session) = "Incomplete consumption of session: " ++ show session
  show (UndefinedProtocol role) = "Role " ++ show role ++ " has no defined protocol"
  show (RoleMismatch role) = "Mismatched Role " ++ show role
  show (UndefinedAlias (STypeIdentifier session)) = "Session " ++ show session ++ " has no defined type alias"
  show (ExpectedPID ty) = "Type error for type " ++ show ty ++ ". Expected PID"
type TypeCheck = Either Error


-- HELPER FUNCTIONS
-- Compare two Types and returns Type if equal
compareTypes :: [TypeAlias] -> Type -> Type -> TypeCheck Type
compareTypes tA (TAny) t            = return t
compareTypes tA t (TAny)            = return t
compareTypes tA (EPid s1) (EPid s2) = compareSessions tA s1 s2 >>= \s -> return $ EPid s
compareTypes tA t1 t2               = if t1 /= t2 then throwError (IncompatibleTypes t1 t2)
                                      else return t1


-- Compares two SessionTypes and returns SessionType if equal
compareSessions :: [TypeAlias] -> SessionType -> SessionType -> TypeCheck SessionType
compareSessions tA SAny s = return s
compareSessions tA s SAny = return s
compareSessions tA s1 s2 = getAlias tA s1 >>= resolveSessionType >>= \s1' ->
  getAlias tA s2 >>= resolveSessionType >>= \s2' ->
  if s1' /= s2' then throwError (SessionMismatch s1 s2) else return s1



-- Returns SessionType of input Actor from list of ActorDefs
getSessionTypeOfActor :: [ActorDef] -> Actor -> TypeCheck SessionType
getSessionTypeOfActor [] actor = throwError (ActorNotDefined actor)
getSessionTypeOfActor ((EActorDef (a, s, _)):ax) actor =
  if a == actor
    then return s
  else getSessionTypeOfActor ax actor


-- Returns SessionType of input Role from list of Protocols
getSessionTypeOfRole :: [Protocol] -> Role -> TypeCheck SessionType
getSessionTypeOfRole [] role = throwError (UndefinedProtocol role)
getSessionTypeOfRole ((Protocol (role', session')):xs) role = do
  if role == role'
    then return session'
  else getSessionTypeOfRole xs role



-- Returns SessionType for input SessionAlias
getAlias :: [TypeAlias] -> SessionType -> TypeCheck SessionType
getAlias [] (STypeIdentifier session) = throwError (UndefinedAlias (STypeIdentifier session))
getAlias ((SessionTypeAlias (sessionName, sessionType)):xs) (STypeIdentifier session) = do
  if sessionName == (STypeIdentifier session)
    then return sessionType
  else getAlias xs (STypeIdentifier session)
getAlias _ session = return session



-- Returns (Type, SessionType) of matching Role, Label in CONNECT SessionAction
getConnectAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getConnectAction (SAction []) role label = throwError (BranchError role label "Connect")
getConnectAction (SAction (x:xs)) role label = do
  case x of
    ((SConnect role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getConnectAction (SAction xs) role label
    _  -> getConnectAction (SAction xs) role label
getConnectAction _ role label = throwError (BranchError role label "Connect")


-- Returns (Type, SessionType) of matching Role, Label in SEND SessionAction
getSendAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getSendAction (SAction []) role label = throwError (BranchError role label "Send")
getSendAction (SAction (x:xs)) role label = do
  case x of
    ((SSend role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getSendAction (SAction xs) role label
    _  -> getSendAction (SAction xs) role label
getSendAction _ role label = throwError (BranchError role label "Send")


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
getAcceptAction (SAction []) role label = throwError (BranchError role label "Accept")
getAcceptAction (SAction (x:xs)) role label = do
  case x of
    ((SAccept role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getAcceptAction (SAction xs) role label
    _  -> getAcceptAction (SAction xs) role label
getAcceptAction _ role label = throwError (BranchError role label "Accept")



getReceiveAction :: SessionType -> Role -> Label -> TypeCheck (Type, SessionType)
getReceiveAction (SAction []) role label = throwError (BranchError role label "Receive")
getReceiveAction (SAction (x:xs)) role label = do
  case x of
    ((SReceive role' label' typeA),session) ->
      if role == role' && label == label'
        then return (typeA,session)
      else getReceiveAction (SAction xs) role label
    _  -> getReceiveAction (SAction xs) role label
getReceiveAction _ role label = throwError (BranchError role label "Receive")



-- Returns list of (Type, SessionType) of all the branches/choices in ACCEPT SessionAction
getAcceptBranches :: State -> Role -> Choices -> TypeCheck [(Type, SessionType)]
getAcceptBranches state role choices = do
  actionsList <- mapM (\(label, binder, computation) ->
    getAlias (typeAliases state) (session state) >>= \session ->
    getAcceptAction session role label >>= \act ->
    return (binder, act, computation)) choices
  results <- mapM (\(binder, (tyI, sessionI), computation) ->
    checkComputation state{typeEnv=(binder,tyI):(typeEnv state), session=sessionI} computation)
    actionsList
  return results


-- Returns list of (Type, SessionType) of all the branches/choices in RECEIVE SessionAction
getReceiveBranches :: State -> Role -> Choices -> TypeCheck [(Type, SessionType)]
getReceiveBranches state role choices = do
  actionsList <- mapM (\(label, value, computation) ->
    getAlias (typeAliases state) (session state) >>= \session ->
    getReceiveAction session role label >>= \act ->
    return (value, act, computation)) choices
  results <- mapM (\(binder, (tyI, sessionI), computation) ->
    checkComputation state{typeEnv=(binder,tyI):(typeEnv state), session=sessionI} computation)
    actionsList
  return results


-- Checks if all the (Type, SessionType) are same and returns tuple
checkAllEqual :: [TypeAlias] -> [(Type, SessionType)] -> TypeCheck (Type, SessionType)
checkAllEqual tA xs = do
  let checkList = map (== head xs) (tail xs)
  if and checkList == True
    then return $ head xs
  else do
    let (ty, session)  = head xs
    let (ty',session') = (tail xs) !! maybe 0 id (elemIndex False checkList)
    t <- compareTypes tA ty ty'
    s <- compareSessions tA session session'
    return (t,s)




unwrapPID :: Type -> TypeCheck SessionType
unwrapPID (EPid session) = return session
unwrapPID t = throwError (ExpectedPID t)


resolveSessionType :: SessionType -> TypeCheck SessionType
resolveSessionType (SRecursion x sessionType) =
  return $ substST sessionType (SRecursion x sessionType) x
resolveSessionType s = return s


substST :: SessionType -> SessionType -> RecursionVar -> SessionType
substST (SEnd) _ _             = SEnd
substST (SDisconnect role) _ _ = (SDisconnect role)
substST (SRecursionVar y) recSession x  = if y == x then recSession else (SRecursionVar y)
substST (SRecursion y sessionType) recSession x = (SRecursion y (substST sessionType recSession x))
substST (SAction actionChoices) recSession x =
  SAction $ map (\(sessionAction, sessionType) -> (sessionAction, substST sessionType recSession x)) actionChoices






checkBehaviour :: State -> Behaviour -> TypeCheck ()
-- T-STOP
checkBehaviour state (EStop) = return ()


checkBehaviour state (EComp comp) = do
  (ty, session') <- checkComputation state comp
  if session' == SEnd
    then return ()
  else throwError (SessionConsumptionError (session state))



-- VALUE TYPING
checkValue :: TypeEnv -> EValue -> TypeCheck Type
-- T-UNIT
checkValue _ EUnit       = return Unit
-- T-STRING
checkValue _ (EString _) = return TString
-- T-BOOL
checkValue _ (EBool _)   = return TBool
-- T-INT
checkValue _ (EInt _)    = return TInt

-- T-VAR
checkValue typeEnv (EVar string) = do
  let returnType = Map.lookup string $ Map.fromList typeEnv in
    case returnType of
      Just t -> return t
      Nothing -> throwError (UnboundVariable string)

-- T-BODY
checkComputation :: State -> Computation -> TypeCheck (Type, SessionType)
-- FUNCTIONAL RULES
-- T-LET
checkComputation state (EAssign binder c1 c2) = do
  (ty, session') <- checkComputation state c1
  (ty', session'') <- checkComputation state {typeEnv=(binder,ty):(typeEnv state), session=session'} c2
  return (ty', session'')

-- T-REC
checkComputation state (ERecursion label comp) = do
  ses <- resolveSessionType (session state)
  (ty, session') <- checkComputation state{recEnv=((label,(session state)):(recEnv state)), session=ses} comp
  return (ty, session')

-- T-CONTINUE
checkComputation state (EAct (EContinue label)) = do
  let returnType = Map.lookup label $ Map.fromList (recEnv state)
  case returnType of
    Just t -> return (TAny, SAny)
    Nothing -> throwError (UnboundRecursionVariable label)

-- T-RETURN
checkComputation state (EAct (EReturn value)) = do
  typeV <- checkValue (typeEnv state) value
  return (typeV, (session state))


-- T-CONDITION
checkComputation state (ECondition val comp1 comp2) = do
  ty <- checkValue (typeEnv state) val
  compareTypes (typeAliases state) ty TBool
  (ty1, ses1) <- checkComputation state comp1
  (ty2, ses2) <- checkComputation state comp2
  ty' <- compareTypes (typeAliases state) ty1 ty2
  ses' <- compareSessions (typeAliases state) ses1 ses2
  return (ty', ses')


-- Checks if parametrized action is a valid comparison action,
-- checks if both values are of the same type and returns the type
checkComputation state (EAct (EEquality val1 val2)) = do
  ty1 <- checkValue (typeEnv state) val1
  ty2 <- checkValue (typeEnv state) val2
  ty <- compareTypes (typeAliases state) ty1 ty2
  return (TBool, (session state))

checkComputation state (EAct (EInequality val1 val2)) = do
  ty1 <- checkValue (typeEnv state) val1
  ty2 <- checkValue (typeEnv state) val2
  ty <- compareTypes (typeAliases state) ty1 ty2
  return (TBool, (session state))


-- ACTOR / ADAPTATION RULES
-- T-NEW
checkComputation state (EAct (ENew actor)) = do
  session' <- getSessionTypeOfActor (actorDefs state) actor
  session' <- getAlias (typeAliases state) session'
  return (EPid session', (session state))

-- T-SELF
checkComputation state (EAct (ESelf)) = getAlias (typeAliases state) (followST state) >>= \f ->
  return (EPid f, (session state))

-- T-DISCOVER
checkComputation state (EAct (EDiscover s)) = getAlias (typeAliases state) s >>= \s ->
  return (EPid s, (session state))

-- T-REPLACE
checkComputation state (EAct (EReplace value behaviour)) = do
  typeV <- checkValue (typeEnv state) value
  case typeV of
    (EPid sessionU) -> checkBehaviour (state {followST = sessionU}) behaviour
    _               -> throwError (ExpectedPID typeV)
  return (Unit, (session state))

-- EXCEPTION HANDLING
-- T-RAISE
checkComputation state (EAct (ERaise)) = return (TAny, SAny)

-- T-TRY
checkComputation state (ETry action comp) = do
  (ty1, session1) <- checkComputation state (EAct (action))
  (ty2, session2) <- checkComputation state comp
  ty <- compareTypes (typeAliases state) ty1 ty2
  session <- compareSessions (typeAliases state) session1 session2
  return (ty,session)

-- SESSION COMMUNICATION RULES
-- T-CONN
checkComputation state (EAct (EConnect label valueV valueW role)) = do
  (typeA, session') <- getConnectAction (session state) role label
  typeV <- checkValue (typeEnv state) valueV
  typeW <- checkValue (typeEnv state) valueW
  sessionW <- unwrapPID typeW
  sessionType' <- getSessionTypeOfRole (protocols state) role

  compareTypes (typeAliases state) typeV typeA
  compareSessions (typeAliases state) sessionW sessionType'
  return (Unit, session')


-- T-SEND
checkComputation state (EAct (ESend label value role)) = do
  (typeA, session') <- getSendAction (session state) role label
  typeV <- checkValue (typeEnv state) value

  compareTypes (typeAliases state) typeV typeA
  return (Unit, session')


-- T-ACCEPT
checkComputation state (EAct (EAccept role choices)) = do
  branches <- getAcceptBranches state role choices
  (ty, session) <- checkAllEqual (typeAliases state) branches
  return (ty, session)

-- T-RECV
checkComputation state (EAct (EReceive role choices)) = do
  branches <- getReceiveBranches state role choices
  (ty, session) <- checkAllEqual (typeAliases state) branches
  return (ty, session)


-- T-WAIT
checkComputation state (EAct (EWait role)) = do
  (ty, session) <- getWaitAction (session state) role
  return (ty, session)

-- T-DISCONN
checkComputation state (EAct (EDisconnect role)) = do
  case (session state) of
    (SDisconnect role') ->
      if role == role' then return (Unit, SEnd)
      else throwError (RoleMismatch role)
    _ -> throwError (SessionConsumptionError (session state))


-- T-SEQUENCE
checkComputation state (ESequence comp1 comp2) = do
  (ty, session)   <- checkComputation state comp1
  -- compareTypes (typeAliases state) ty Unit
  (ty', session') <- checkComputation state{session=session} comp2
  return (ty', session')





-- T-DEF
checkActorDef :: State -> ActorDef -> TypeCheck ()
checkActorDef state (EActorDef (actor, session, computation)) = do
  session'<- getAlias (typeAliases state) session
  (ty, session'') <- checkComputation state{followST=session', session=session'} computation
  if session'' == SEnd || session'' == SAny then return ()
  else throwError (SessionConsumptionError session)


-- T-PROGRAM
checkProgram :: Program -> TypeCheck ()
checkProgram (typeAliases, actorDefs, protocols, computation) = do
  let state = State { actorDefs=actorDefs
                      , typeAliases=typeAliases
                      , protocols=protocols
                      , typeEnv=[], recEnv=[]}
  mapM_ (checkActorDef state) actorDefs
  checkComputation state{followST=SEnd, session=SEnd} computation
  return ()