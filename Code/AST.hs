module AST where

-- ABSTRACT SYNTAX TREE
type Label = String
type Binder = String
type Choices = [(Label, Binder, Computation)]
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
  | EString String
  | EInt Int
  | EBool Bool
  | EUnit
  deriving (Eq, Ord, Show)
-- Actions
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
  | EEquality EValue EValue
  | EInequality EValue EValue
  deriving (Show)
-- Computations
data Computation = EAssign Binder Computation Computation
  | ETry EAction Computation
  | ERecursion Label Computation
  | EAct EAction
  | ECondition EValue Computation Computation
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
newtype TypeAlias = SessionTypeAlias (SessionType, SessionType) deriving (Show)
-- Actor Definition
newtype ActorDef = EActorDef (Actor, SessionType, Computation) deriving (Show)
-- Protocol
newtype Protocol = Protocol (Role, SessionType) deriving (Show)
-- Program
type Program = ([TypeAlias], [ActorDef], [Protocol], Computation)