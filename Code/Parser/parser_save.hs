{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes #-}
{-#LANGUAGE ScopedTypeVariables#-}

import Text.RawString.QQ
import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L



type Parser = Parsec Void Text

sc :: Parser ()                  -- Space Consumer
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a   -- Trailing space
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

rws = ["return", "continue", "raise", "new", "self", "let", "in", "replace", "with"
      , "discover", "connect", "to", "as", "accept", "from", "send", "receive", "wait"
      , "disconnect", "try", "catch", "stop", "end", "boot", "Pid", "unit"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> letterChar <*> many alphaNumChar
   check x =
     if x `elem` rws
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

pUpperCase :: Parser String
pUpperCase = (lexeme . try) p
  where p = (:) <$> upperChar <*> many alphaNumChar


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
  | Unit deriving (Show)
-- Values
data EValue = EVar String
  | EInt String
  | EBool Bool
  | EUnit
  deriving (Show)
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




pVValue :: Parser EValue
pVValue = EVar <$> identifier

pVUnit :: Parser EValue
pVUnit = do
  reserved "()"
  return EUnit

pVInt :: Parser EValue
pVInt = do
  int <- many digitChar
  return $ EInt int

pVBoolTrue :: Parser EValue
pVBoolTrue = do
  string' "true" *> notFollowedBy alphaNumChar *> sc
  return $ EBool True

pVBoolFalse :: Parser EValue
pVBoolFalse = do
  string' "false" *> notFollowedBy alphaNumChar *> sc
  return $ EBool False

pValue :: Parser EValue
pValue = choice
  [ pVValue
  , pVUnit
  , pVInt
  , pVBoolTrue
  , pVBoolFalse
  ]




pEpid :: Parser Type
pEpid = do
  reserved "Pid"
  sessionType <- parens pSessionType
  return $ EPid sessionType

pUnit :: Parser Type
pUnit = do
  reserved "Unit"
  return Unit

pString :: Parser Type
pString = do
  reserved "String"
  return TString

pInt :: Parser Type
pInt = do
  reserved "Int"
  return TInt

pBool :: Parser Type
pBool = do
  reserved "Bool"
  return TBool

pType :: Parser Type
pType = choice
  [ pEpid
  , pString
  , pInt
  , pBool
  , pUnit
  ]



pReturn :: Parser EAction
pReturn = try $ do
  reserved "return"
  value <- pValue
  return $ EReturn value

pContinue :: Parser EAction
pContinue = try $ do
  reserved "continue"
  label <- identifier
  return $ EContinue label

pRaise :: Parser EAction
pRaise = try $ do
  reserved "raise"
  return ERaise

pNew :: Parser EAction
pNew = try $ do
  reserved "new"
  actor <- pUpperCase
  return $ ENew actor

pSelf :: Parser EAction
pSelf = try $ do
  reserved "self"
  return ESelf

pReplace :: Parser EAction
pReplace = try $ do
  reserved "replace"
  value <- pValue
  reserved "with"
  behaviour <- pBehaviour
  return $ EReplace value behaviour

pDiscover :: Parser EAction
pDiscover = try $ do
  reserved "discover"
  sessionType <- pSTypeIdentifier
  return $ EDiscover sessionType

pConnect :: Parser EAction
pConnect = try $ do
  reserved "connect"
  label <- identifier
  vValue <- parens pValue
  reserved "to"
  wValue <- pValue
  reserved "as"
  roleName <- identifier
  return $ EConnect label vValue wValue roleName


pChoice :: Parser (Label, EValue, Computation)
pChoice = try $ do
  label <- identifier
  value <- parens pValue
  reserved "->"
  computation <- pComputation
  return (label, value, computation)

pAcceptBraces :: Parser EAction
pAcceptBraces = try $ do
  reserved "accept from"
  roleName <- identifier
  choices <- braces $ many pChoice
  return $ EAccept roleName choices

pReceiveBraces :: Parser EAction
pReceiveBraces = try $ do
  reserved "receive from"
  roleName <- identifier
  choices <- braces $ many pChoice
  return $ EReceive roleName choices

pReceiveSingle :: Parser EAction
pReceiveSingle = try $ do
  reserved "receive"
  label <- identifier
  value <- parens pValue
  reserved "from"
  role <- identifier
  reserved ";"
  computation <- pComputation
  return $ EReceive role [(label, value, computation)]

pAcceptSingle :: Parser EAction
pAcceptSingle = try $ do
  reserved "accept"
  label <- identifier
  value <- parens pValue
  reserved "from"
  role <- identifier
  reserved ";"
  computation <- pComputation
  return $ EAccept role [(label, value, computation)]

pReceive :: Parser EAction
pReceive = pReceiveBraces <|> pReceiveSingle

pAccept :: Parser EAction
pAccept = pAcceptBraces <|> pAcceptSingle


pSend :: Parser EAction
pSend = try $ do
  reserved "send"
  label <- identifier
  value <- parens pValue
  reserved "to"
  role <- identifier
  return $ ESend label value role

pWait :: Parser EAction
pWait = try $ do
  reserved "wait"
  roleName <- identifier
  return $ EWait roleName

pDisconnect :: Parser EAction
pDisconnect = try $ do
  reserved "disconnect from"
  role <- identifier
  return $ EDisconnect role

pAction :: Parser EAction
pAction = choice
  [ pReturn
  , pContinue
  , pRaise
  , pNew
  , pSelf
  , pReplace
  , pDiscover
  , pConnect
  , pAccept
  , pSend
  , pReceive
  , pWait
  , pDisconnect ]



pAssign :: Parser Computation
pAssign = try $ do
  reserved "let"
  binder <- identifier
  reserved "<="
  computationM <- pComputation
  reserved "in"
  computationN <- pComputation
  return $ EAssign binder computationM computationN

pTry :: Parser Computation
pTry = try $ do
  reserved "try"
  action <- pAction
  reserved "catch"
  computation <- pComputation
  return $ ETry action computation

pRecursion :: Parser Computation
pRecursion = try $ do
  label <- identifier
  reserved "::"
  computation <- pComputation
  return $ ERecursion label computation

pAct :: Parser Computation
pAct = EAct <$> pAction

pSequence :: Parser Computation
pSequence = try $ do
  computation1 <- pNonseqComputation
  reserved ";"
  computation2 <- pComputation
  return $ ESequence computation1 computation2

pNonseqComputation :: Parser Computation
pNonseqComputation = choice
  [ pAssign
  , pTry
  , pRecursion
  , pAct ]

pComputation :: Parser Computation
pComputation = choice
  [ pSequence
  , pNonseqComputation ]



pStop :: Parser Behaviour
pStop = try $ do
  reserved "stop"
  return $ EStop

pComp :: Parser Behaviour
pComp = try $ do
  computation <- pComputation
  return $ EComp computation

pBehaviour :: Parser Behaviour
pBehaviour = choice
  [ pComp
  , pStop ]





pSSend :: Parser SessionAction
pSSend = try $ do
  role <- identifier
  symbol "!"
  label <- identifier
  typeName <- parens pType
  return $ SSend role label typeName

pSConnect :: Parser SessionAction
pSConnect = try $ do
  role <- identifier
  symbol "!!"
  label <- identifier
  typeName <- parens pType
  return $ SConnect role label typeName

pSReceive :: Parser SessionAction
pSReceive = try $ do
  role <- identifier
  symbol "?"
  label <- identifier
  typeName <- parens pType
  return $ SReceive role label typeName

pSAccept :: Parser SessionAction
pSAccept = try $ do
  role <- identifier
  symbol "??"
  label <- identifier
  typeName <- parens pType
  return $ SAccept role label typeName

pSWait :: Parser SessionAction
pSWait = try $ do
  symbol "#"
  role <- identifier
  return $ SWait role

pSessionAction :: Parser SessionAction
pSessionAction = choice
   [ pSConnect
   , pSSend
   , pSAccept
   , pSReceive
   , pSWait ]



pSRecursion :: Parser SessionType
pSRecursion = try $ do
  reserved "rec"
  recVar <- identifier
  symbol "."
  sessionType <- pSessionType
  return $ SRecursion recVar sessionType

pSRecursionVar :: Parser SessionType
pSRecursionVar = do
  recursionVar <- identifier
  return $ SRecursionVar recursionVar

pSDisconnect :: Parser SessionType
pSDisconnect = try $ do
  symbol "##"
  role <- identifier
  return $ SDisconnect role

pSEnd :: Parser SessionType
pSEnd = try $ do
  reserved "end"
  return SEnd

pSTypeIdentifier :: Parser SessionType
pSTypeIdentifier = try $ do
  sessionTypeName <- pUpperCase
  return $ STypeIdentifier sessionTypeName


pSSAction :: Parser (SessionAction, SessionType)
pSSAction = try $ do
  sessionAction <- pSessionAction
  symbol "."
  sessionType <- pSessionType
  return (sessionAction, sessionType)


plusSep p = p `sepBy1` (symbol "+")


pSSingleAction :: Parser SessionType
pSSingleAction = try $ do
  actionChoice <- pSSAction
  return $ SAction [actionChoice]

pSManyActions :: Parser SessionType
pSManyActions = try $ do
  actionChoices <- plusSep (parens pSSAction)
  return $ SAction actionChoices

pSAction :: Parser SessionType
pSAction = pSManyActions <|> pSSingleAction


pSessionType :: Parser SessionType
pSessionType = choice 
  [ pSAction
  , pSRecursion
  , pSTypeIdentifier
  , pSRecursionVar
  , pSDisconnect
  , pSEnd ]



pActorDef :: Parser ActorDef
pActorDef = try $ do
  reserved "actor"
  actor <- pUpperCase
  reserved "follows"
  session <- parens pSessionType
  computation <- braces pComputation
  return $ EActorDef actor session computation



pTypeAlias :: Parser TypeAlias
pTypeAlias = try $ do
  reserved "type"
  sessionTypeName <- pSTypeIdentifier
  reserved "="
  session <- pSessionType
  return $ SessionTypeAlias sessionTypeName session



pProtocol :: Parser Protocol
pProtocol = try $ do
  role <- pUpperCase
  reserved ":"
  session <- pSessionType
  return $ Protocol role session



pMainComputation :: Parser Computation
pMainComputation = try $ do
  reserved "boot"
  computation <- braces pComputation
  return computation



pProgram :: Parser Program
pProgram = do
  typeAliases <- many pTypeAlias
  actorDefs <- many pActorDef
  protocols <- many pProtocol
  computation <- pMainComputation
  return (typeAliases, actorDefs, protocols, computation)


main = do
  let test = [r|actor PingerActor follows (Ponger !! ping(unit) . Ponger ? pong(unit) . #Ponger . rec browse . Pinger !! pong(unit) . browse) {
  let pid <= discover Ponger in
  connect ping(()) to pid as Ponger;
  receive from Ponger {
      pong(x) -> wait Ponger
      ping(x) -> raise
  };
  browse ::
  raise; self;
  continue browse
}

actor PongerActor follows (Pinger ?? ping(unit) . Pinger ! pong(unit) . ##Ponger) {
  accept from Pinger {
      ping(()) ->
          send pong(()) to Pinger;
          disconnect from Pinger
      pong(()) ->
        raise
  }
}

Pinger : Ponger

boot {
  raise
}

|]
  let test1 = [r|Pinger ?? ping(unit) . Pinger ! pong(unit) . ##Ponger|]
  parseTest pSessionType test1
  parseTest pProgram test