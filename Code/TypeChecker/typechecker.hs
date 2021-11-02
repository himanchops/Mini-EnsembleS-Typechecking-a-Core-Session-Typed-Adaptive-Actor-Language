{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import qualified Data.Map as Map








type Label = String
type Role = String
type Binder = String
data Choice = SChoice Label EValue Computation deriving (Show)
type Choices = [Choice]
type Actor = String
data ActorDef = EActorDef Actor SessionType Computation deriving (Show)
data Behaviour = EComp Computation | EStop deriving (Show)
data Type = EPid SessionType | Unit deriving (Show)

data EValue = VVar String | EUnit deriving (Show)
data EAction = EReturn EValue
  | EContinue Label
  | ERaise
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
data Computation = EAssign Binder Computation Computation
  | ETry EAction Computation
  | ERecursion Label Computation
  | EAct EAction
  | ESequence Computation Computation
  deriving (Show)


type RecursionVar = String
type SessionTypeName = String
data SessionAction = SSend Role Label Type
  | SConnect Role Label Type
  | SReceive Role Label Type
  | SAccept Role Label Type
  | SWait Role
  deriving (Show)
data ActionChoice = SAction SessionAction SessionType deriving (Show)
data SessionType = SSequence SessionType SessionType
  | SSingleSession ActionChoice
  | SRecursion RecursionVar SessionType
  | SRecursionVar RecursionVar
  | SDisconnect Role
  | SEnd
  | STypeIdentifier SessionTypeName
  deriving (Show)

data TypeAlias = SessionTypeAlias SessionType SessionType deriving (Show)
data Protocol = Protocol Role SessionType deriving (Show)
type Program = ([TypeAlias], [ActorDef], [Protocol], Computation)












{-
Check Program :: Program → TC ()
Check Definition :: Definition → TC()
Check Protocol :: Protocol → TC()
Check Computation :: SessionType → TypeEnv → SessionType → Computation → TC(Type, SessionType)
Check Value :: TypeEnv → Value → TC Type
-}


{-
compareTypes :: Type -> Type -> TC Type
compareTypes Any t = return t
compareTypes t Any = return t
compareTypes t1 t2 = if t1 <> t2 then throwError (IncompatibleTypes t1 t2) else return t1

ERaise Type 


echo :: IO () echo = do line <- getLine putStrLn line return () 
echo = getLine >>= (\line -> putStrLn line >>= (\() -> return ())) 

https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
http://www.dcs.gla.ac.uk/~ornela/projects/Artem%20Usov.pdf


MONADS
do e1 ; e2      =        e1 >> e2
do p <- e1; e2  =        e1 >>= \p -> e2
do p <- e1; e2  =   e1 >>= (\v -> case v of p -> e2; _ -> fail "s")

return a >>= k	=	k a
m >>= return	=	m
xs >>= return . f	=	fmap f xs
m >>= (\x -> k x >>= h)	=	(m >>= k) >>= h

If you have a need to store some global state which various of your functions modify,
then you have a design issue.
The correct solution in C is to pass a 'bundle' of appropriate parameters
(you might call it an environment) to those functions which need it.
In C++, perl or python you'd very likely make this bundle an object.
In haskell this bundle will be a data value in some custom type;
and using Monads you can 'hide the details' of passing it around.
TypeEnv, RecEnv

-}

type TypeEnv = [(String, Type)]
type RecEnv = [(String, SessionType)]


data Error = UnboundVariable String
        | TypeMismatch Type Type
        | UnboundRecursionVariable String
        | RecursionVariableMismatch String Type Type 
-- Different types of errors?


instance Show Error where
  show (UnboundVariable value) = "Unbounded variable: " ++ (show value)

type TypeCheck = Either Error



-- VALUE TYPING
checkValue :: TypeEnv -> EValue -> TypeCheck Type
-- T-UNIT
checkValue typeEnv (EUnit) = return(Unit)

-- T-VAR
checkValue typeEnv (VVar value) = do
  let returnType = Map.lookup value $ Map.fromList typeEnv
  case returnType of
    Just t -> return(t)
    Nothing -> throwError (UnboundVariable value)



checkComputation :: SessionType -> TypeEnv -> RecEnv-> SessionType -> Computation -> TypeCheck (Type, SessionType)
-- FUNCTIONAL RULES
-- T-LET
checkComputation followST typeEnv recEnv session (EAssign binder c1 c2) = do
  (ty, session') <- checkComputation followST typeEnv recEnv session c1
  (ty', session'') <- checkComputation followST ((binder,ty):typeEnv) recEnv session' c2
  return (ty', session'')

-- T-REC
checkComputation followST typeEnv recEnv session (ERecursion label comp) = do
  (ty, session') <- checkComputation followST typeEnv ((label,session):recEnv) session comp
  return (ty, session')

-- T-CONTINUE

-- T-RETURN
checkComputation followST typeEnv recEnv session (EAct (EReturn value)) = do
  typeV <- checkValue typeEnv value
  return (typeV, session)

-- ACTOR / ADAPTATION RULES
-- T-NEW
-- explicit actor u follows S {M}, u:S,M?? S=sessionType(u), M=behaviour(u)

-- T-SELF
checkComputation followST typeEnv recEnv session (EAct(ESelf)) = do
  return (EPid followST, session)

-- T-DISCOVER
-- checkComputation followST typeEnv recEnv session (EAct(EDiscover s) = do
--   return (EPid s, session)

-- T-REPLACE
-- Γ ⊢ V :Pid(U), {U} Γ ⊢ κ
-- DOES THIS MEAN EXTEND/UPDATE THE TYPING ENVIRONMENT or CHECK if it exists?
-- 

-- EXCEPTION HANDLING
-- T-RAISE
-- TYPE A? Int? String? Any? What is TYPE exactly? Like we previously defined Pid(S)|1 or something else

-- T-TRY
-- typeAction (try L catch M)


-- SESSION COMMUNICATION RULES
-- T-CONN
{- STEPS : connect label(value1) to value2 as role
   1. Check if <role> !! label (type) exists in single-sessions
   2. checkValue value1, should match types with type, V:A
   3. checkValue value2, should match Pid(followST), W:Pid(T)
   4. T = sessionType of role
   5. return 1 and corresponding updated sessionType

HOW TO MOVE ON TO THE NEXT SESSION AFTER THIS ACTION MANUALLY? BREAK AND FEED NEXT?
-}

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



-- main = do
--   let session = STypeIdentifier <$> "Pinger"
--   result <- checkComputation session [] [] session (EAct (EReturn "int"))
--   show result