{-# LANGUAGE OverloadedStrings #-}

module HI.Parser
  ( parse
  ) where

import Control.Monad.Combinators (count)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, eof, many, manyTill, notFollowedBy, satisfy, sepBy,
                        sepBy1, sepEndBy, some, try, (<|>))
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, hexDigitChar, space, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral, scientific)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Read (readMaybe)

import HI.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parses given input and returns HiExpr on success and ParseErrorBundle
-- if parsing fails.
--
-- Parsing is done via the following grammar (simplified to omit technical details):
--
-- Result -> InfixExpression EOF
-- Term   -> AnyCallChain | '(' InfixExpression ')'
--
-- AnyCallChain -> Expression (DotCall | ActionCall | FunctionCall)*
-- DotCall      -> '.' (alpha alphaNum*)+ ('-' alpha alphaNum*)*
-- ActionCall   -> '!'
-- FunctionCall -> '(' Arguments ')'
-- Arguments    -> eps | InfixExpression (',' InfixExpression)*
--
-- Expression -> Value | MapDeclaration | ListDeclaration
-- Value      -> number | boolean | string | null | function | bytesstring | (cwd | now)
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = Text.Megaparsec.parse parser ""

parser :: Parser HiExpr
parser = parseInfixExpression <* eof

parseValue :: Parser HiValue
parseValue =
  space
    *> choice
      [ HiValueNumber <$> rational,
        HiValueBool <$> boolean,
        HiValueString <$> stringLiteral,
        HiValueNull <$ nullLiteral,
        HiValueFunction <$> parseFunction,
        HiValueBytes <$> bytes,
        HiValueAction <$> (cwd <|> now)
      ]
    <* space

parseInfixExpression :: Parser HiExpr
parseInfixExpression = lexeme $ makeExprParser (between space space term) table

term :: Parser HiExpr
term = lexeme $ try parseAnyCallChain <|> parens parseInfixExpression

table :: [[Operator Parser HiExpr]]
table =
  [ [ binaryL "*" HiFunMul,
      binaryLNotFollowedBy "/" "=" HiFunDiv
    ],
    [ binaryL "+" HiFunAdd,
      binaryL "-" HiFunSub
    ],
    [ binaryN "<=" HiFunNotGreaterThan,
      binaryN ">=" HiFunNotLessThan,
      binaryN "<" HiFunLessThan,
      binaryN ">" HiFunGreaterThan,
      binaryN "==" HiFunEquals,
      binaryN "/=" HiFunNotEquals
    ],
    [binaryR "&&" HiFunAnd],
    [binaryR "||" HiFunOr]
  ]

makeBinaryHiFunction :: HiFun -> HiExpr -> HiExpr -> HiExpr
makeBinaryHiFunction f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

binaryL, binaryN, binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL (makeBinaryHiFunction f <$ symbol name)
binaryN name f = InfixN (makeBinaryHiFunction f <$ symbol name)
binaryR name f = InfixR (makeBinaryHiFunction f <$ symbol name)

binaryLNotFollowedBy :: String -> String -> HiFun -> Operator Parser HiExpr
binaryLNotFollowedBy name nb f = InfixL $ makeBinaryHiFunction f <$ try (symbol name <* notFollowedBy (string nb))

parseExpression :: Parser HiExpr
parseExpression = choice
  [ HiExprValue <$> parseValue
  , HiExprDict <$> mapDeclaration
  , HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> list
  ]

parseAnyCallChain :: Parser HiExpr
parseAnyCallChain = lexeme $ do
  fc <- parseExpression <|> parens parseInfixExpression
  parseCallChain fc

parseCallChain :: HiExpr -> Parser HiExpr
parseCallChain e = lexeme $ do
  ((parseDotCallChain e <|> parseActionCallChain e <|> parseFunctionCallChain e) >>= parseCallChain) <|> return e

parseDotCallChain :: HiExpr -> Parser HiExpr
parseDotCallChain fc = do
  dotCalls <- some (char '.' *> parseDotAccessString)
  return $ foldl (\e dc -> HiExprApply e [dc]) fc dotCalls

parseActionCallChain :: HiExpr -> Parser HiExpr
parseActionCallChain fc = do
  actionCalls <- some $ symbol "!"
  return $ foldl (\e _ -> HiExprRun e) fc actionCalls

parseFunctionCallChain :: HiExpr -> Parser HiExpr
parseFunctionCallChain fc = do
  argsCalls <- some parseArgs
  return $ foldl HiExprApply fc argsCalls

parseDotAccessString :: Parser HiExpr
parseDotAccessString = HiExprValue . HiValueString . T.pack . intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

parseFunction :: Parser HiFun
parseFunction =
  choice
    [ HiFunAdd <$ string "add",
      HiFunSub <$ string "sub",
      HiFunMul <$ string "mul",
      HiFunDiv <$ string "div",
      HiFunNotEquals <$ string "not-equals",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNot <$ string "not",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo",
      HiFunCount <$ string "count",
      HiFunKeys <$ string "keys",
      HiFunValues <$ string "values",
      HiFunInvert <$ string "invert"
    ]

stringLiteral :: Parser Text
stringLiteral = lexeme $ T.pack <$> (char '"' *> manyTill charLiteral (char '"'))

boolean :: Parser Bool
boolean = lexeme $ False <$ string "false" <|> True <$ string "true"

rational :: Parser Rational
rational = lexeme $ toRational <$> L.signed space scientific

nullLiteral :: Parser String
nullLiteral = symbol "null"

list :: Parser [HiExpr]
list = lexeme $ brackets $ sepBy parseInfixExpression (char ',')

mapEntry :: Parser (HiExpr, HiExpr)
mapEntry = lexeme $ do
  key <- parseInfixExpression <* char ':'
  value <- parseInfixExpression
  return (key, value)

mapDeclaration :: Parser [(HiExpr, HiExpr)]
mapDeclaration = lexeme $ braces $ sepBy mapEntry (char ',')

bytes :: Parser ByteString
bytes = lexeme $ do
  bts <- (string "[#" *> space) *> sepEndBy (count 2 hexDigitChar) space1 <* string "#]"
  let hex = (++) "0x" <$> bts
  case sequenceA $ readMaybe <$> hex of
    Just parsedBytes -> return $ B.pack $ toEnum <$> parsedBytes
    Nothing          -> error "Error occured while reading a sequence of bytes" -- Unreachable. Something is working incorrectly

cwd :: Parser HiAction
cwd = lexeme $ HiActionCwd <$ string "cwd"

now :: Parser HiAction
now = lexeme $ HiActionNow <$ string "now"

parseArgs :: Parser [HiExpr]
parseArgs = lexeme $ parens $ sepBy parseInfixExpression (char ',')
