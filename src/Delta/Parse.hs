{-# LANGUAGE OverloadedStrings #-}
module Delta.Parse (delta) where

import Control.Monad (when)
import Data.Void (Void)
import Data.Text (Text)
import Data.List (elem)

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Delta.Lang

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "%")
  (L.skipBlockComment "(-" "-)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

decimal :: Parser Int
decimal = lexeme L.decimal

boolLit :: Parser Bool
boolLit = lexeme $ do
  n <- lexeme L.decimal
  when (n /= 0 && n /= 1) $
    fail "invalid boolean"
  return $ n == 1

listLit :: Parser List
listLit = symbol "[" *> manyTill decimal (symbol "]")

keyword :: Text -> Parser Text
keyword w = lexeme (string w <* notFollowedBy alphaNumChar)

keywords :: [String]
keywords = ["forall", "exists", "let", "in", "tail", "conc"]

varName :: Parser String
varName = lexeme $ do
  name <- (:) <$> letterChar <*> many alphaNumChar <?> "variable"
  when (name `elem` keywords) $
    fail "used a reserved keyword as var name"
  return name

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

delta :: Parser (Expr Bool)
delta = choice
  [ keyword "forall" *> deltaForall
  , keyword "exists" *> deltaExists
  , keyword "let"    *> deltaLet
  , deltaBoolExpr
  ] <* eof

deltaForall :: Parser (Expr Bool)
deltaForall = Forall <$> varName <* keyword "in" <*> deltaList <* keyword "." <*> delta

deltaExists :: Parser (Expr Bool)
deltaExists = Exists <$> varName <* keyword "in" <*> deltaList <* keyword "." <*> delta

deltaLet :: Parser (Expr Bool)
deltaLet = Let <$> varName <* symbol "=" <*> deltaList <* symbol "." <*> delta

deltaBoolExpr :: Parser (Expr Bool)
deltaBoolExpr = makeExprParser term ops where
  term = choice [BConst <$> boolLit, deltaListBoolExpr, parens deltaBoolExpr]
  ops = [ [ Prefix (BNot <$ symbol "!") ]
        , [ InfixL (BOp BAnd <$ symbol "*") ]
        , [ InfixL (BOp BOr  <$ symbol "+") ]
        ]

deltaListBoolExpr :: Parser (Expr Bool)
deltaListBoolExpr = do
  l <- deltaList
  choice
    [ LIn l <$ symbol "in" <*> deltaList
    , LEq l <$ symbol "="  <*> deltaList
    ]

deltaList :: Parser (Expr List)
deltaList = choice
  [ parens deltaList
  , LConst <$> listLit
  , LTail <$ keyword "tail" <*> deltaList
  , LConc <$ keyword "conc" <*> deltaList <*> deltaList
  , Var <$> varName
  ]
