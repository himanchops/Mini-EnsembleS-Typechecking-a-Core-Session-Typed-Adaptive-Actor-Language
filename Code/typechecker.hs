{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


import AST


{-
Check Program :: Program → TC ()
Check Definition :: Definition → TC()
Check Protocol :: Protocol → TC()
Check Computation :: SessionType → TypeEnv → SessionType → Computation → TC(Type, SessionType)
Check Value :: TypeEnv → Value → TC Type


SUBTYPING
Person = (name : String)
Employee = (name: String, department: String)
defName :: Person → String
TypeEnv M:A, A<=B   =>   TypeEnv M:B
-}

-- https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html 





--type TC a = Except Error a
{-
data Error = UnboundVariable Var | TypeMismatch Type Type | UnboundRecursionVariable Var | RecursionVariableMismatch Var Type Type 
showError :: Error -> String
instance Show Error where show (TypeMismatch t1 t2) = ... 

compareTypes :: Type -> Type -> TC Type
compareTypes Any t = return t
compareTypes t Any = return t
compareTypes t1 r2 = if t1 <> t2 then throwError (IncompatibleTypes t1 t2) else return t1

ERaise Type 


echo :: IO () echo = do line <- getLine putStrLn line return () 
echo = getLine >>= (\line -> putStrLn line >>= (\() -> return ())) 

https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
http://www.dcs.gla.ac.uk/~ornela/projects/Artem%20Usov.pdf

-}
type TC a = Either Error a

type typeEnv = [(String, Type)]
type RecEnv = [(String, SessionType)]






-- TypeEnv | V : A
typeValue :: typeEnv -> EValue -> TC Type
typeValue typeEnv value = TC Unit


-- {T} TypeEnv | S ► M : A ◄ S`
typeComputation :: SessionType -> typeEnv -> SessionType -> Computation -> (Type, SessionType)
--Actions


-- {T} TypeEnv | S ► return <value> : unit ◄ S`
typeComputation followST typeEnv ST (EAct(EReturn value)) = do
  typeV <- typeValue typeEnv value
  return (typeV, ST)




-- {T} TypeEnv | S ► continue <label> : unit ◄ S`
typeComputation followST typeEnv ST (EAct(EContinue label)) = TC (Unit, ST)
-- {T} TypeEnv | S ► Raise : unit ◄ S
typeComputation followST typeEnv ST (EAct(ERaise)) = TC (Unit, ST)
-- {T} TypeEnv | S ► new <actor> : unit ◄ S
typeComputation followST typeEnv ST (EAct(ENew actor)) = TC (Unit, ST)
-- {T} TypeEnv | S ► Self : Pid(T) ◄ S`
typeComputation followST typeEnv ST (EAct(ESelf)) = TC (EPid followST, ST)
-- {T} TypeEnv | S ► discover <sessionType> : Pid(<sessionType>) ◄ S`
typeComputation followST typeEnv ST (EAct(EDiscover session)) = TC (EPid session, ST)



-- {T} TypeEnv | S ► wait <role> : unit ◄ S
typeComputation followST typeEnv ST (EAct(EWait Role)) = TC (Unit, ST)
-- {T} TypeEnv | S ► disconnect from <role> : unit ◄ S
typeComputation followST typeEnv ST (EAct(EDisconnect Role)) = TC (Unit, ST)


-- Assignment
typeComputation followST typeEnv ST (EAssign binder c1 c2) = 
  let (ty, ST`) = typeComputation followST typeEnv ST in
  let (ty`, ST``) = typeComputation followST ((binder,ty):typeEnv) ST` in (ty`, ST``)