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