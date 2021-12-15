type Binder = String
-- Actor Name
type Actor = String
-- Roles
type Role = String

-- Behaviours
data Behaviour = EComp Computation | EStop
-- Types
data Type = EPid SessionType
  | TString
  | TInt
  | TBool
  | Unit
-- Values
data EValue = EVar String
  | EInt Int
  | EBool Bool
  | EUnit

type Label = String
type Choices = [(Label, Binder, Computation)]
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
-- Computations
data Computation = EAssign Binder Computation Computation
  | ETry EAction Computation
  | ERecursion Label Computation
  | EAct EAction
  | ECondition EValue Computation Computation
  | ESequence Computation Computation

-- Session Actions
data SessionAction = SSend Role Label Type
  | SConnect Role Label Type
  | SReceive Role Label Type
  | SAccept Role Label Type
  | SWait Role
type RecursionVar = String
type SessionTypeName = String
-- SessionTypes
data SessionType = SAction [(SessionAction, SessionType)]
  | SRecursion RecursionVar SessionType
  | SRecursionVar RecursionVar
  | SDisconnect Role
  | SEnd
  | STypeIdentifier SessionTypeName
  
-- TypeAlias
newtype TypeAlias = SessionTypeAlias (SessionType, SessionType)
-- Actor Definition
newtype ActorDef = EActorDef (Actor, SessionType, Computation)
-- Protocol
newtype Protocol = Protocol (Role, SessionType)
-- Program
type Program = ([TypeAlias], [ActorDef], [Protocol], Computation)