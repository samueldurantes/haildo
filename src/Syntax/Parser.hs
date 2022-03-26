module Syntax.Parser 
  ( parseSExpr
  ) where

import Data.Void (Void)
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Syntax.Tree
import Data.Char (isSymbol)

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

integer :: Parser Integer
integer = label "integer" $ lexeme $
  read <$> some numberChar

bool :: Parser Bool
bool = label "bool" $ lexeme $ 
  False <$ string "false" <|> True <$ string "true"

identifier :: Parser Text
identifier = label "identifier" $ lexeme $ do
  first <- letterChar <|> char '_' <|> char '+'
  rest <- many $ alphaNumChar <|> char '_' <|> char '+'
  pure $ pack $ first : rest

sexpr :: Parser [SExpr]
sexpr = label "s-expression" $ lexeme $
  between (lexeme (char '(')) (char ')') (many atom)

atom :: Parser SExpr
atom = choice
  [ SBool <$> bool
  , SInteger <$> integer
  , SIdentifier <$> identifier 
  , SSExpr <$> sexpr
  ]

parseSExpr :: Text -> Either String [SExpr]
parseSExpr input =
  let
    outputE = parse
      (between skipSpace eof (many atom))
      ""
      input
  in
  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output
