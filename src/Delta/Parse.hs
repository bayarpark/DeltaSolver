{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Delta.Parse (Signatures, delta, peekSignatures) where

import Control.Monad (when, void)
import Data.Void (Void)
import Data.Text (Text)
import Data.List (elem)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Delta.Lang hiding (Formula)
import Delta.Solve (Formulas, Formula(..))
import qualified Delta.Lang as Lang

type Parser = Parsec Void Text

data Signatures = Signatures Env

type Env = [(VarName, VarType)]
type VarName = String
data VarType = TList | TFormula Int deriving Show

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
listLit = symbol "<" *> (manyTill decimal (symbol ">"))

keyword :: Text -> Parser Text
keyword w = lexeme (string w <* notFollowedBy alphaNumChar)

keywords :: [String]
keywords = ["forall", "exists", "let", "in", "tail", "conc", "nil"]

varName :: Parser String
varName = lexeme $ do
  name <- (:) <$> letterChar <*> many alphaNumChar <?> "variable"
  when (name `elem` keywords) $
    fail "used a reserved keyword as var name"
  return name

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

delta :: Signatures -> Parser Formulas
delta (Signatures initialEnv) = M.fromList <$> sepEndBy formula (symbol ";") <* eof
  where
  formula = do
    funcName <- varName
    argNames <- manyTill varName (symbol "=")
    body <- delta' $ (map (, TList) argNames) ++ initialEnv
    return
      ( funcName
      , Formula
          { argNames = argNames
          , expr = body
          }
      )

peekSignatures :: Parser Signatures
peekSignatures =
  Signatures <$> many signature
  where
  signature = do
    funcName <- varName
    argsCount <- length <$> manyTill varName (symbol "=")
    skipManyTill L.charLiteral (void (symbol ";") <|> eof)
    return (funcName, TFormula argsCount)

delta' :: Env -> Parser (ExprL Bool)
delta' env = makeExprParser (deltaTerm env) ops where
  ops = [ [ Prefix (BNot <$ symbol "!") ]
        , [ InfixL (BOp BAnd <$ symbol "*") ]
        , [ InfixL (BOp BOr  <$ symbol "+") ]
        , [ InfixL (impl     <$ symbol "->")]
        ]
  impl e1 e2 = BOp BOr (BNot e1) e2

deltaTerm :: Env -> Parser (ExprL Bool)
deltaTerm env = choice
  [ keyword "forall" *> (deltaForall env)
  , keyword "exists" *> (deltaExists env)
  , keyword "let"    *> (deltaLet env)
  , BConst <$> boolLit
  , parens $ delta' env
  , funcOrVar env
  , deltaListFormula env
  ]

funcOrVar :: Env -> Parser (ExprL Bool)
funcOrVar env = do
  o <- getOffset
  var <- try varName
  case lookup var env of
    Just TList -> deltaListFormula' env (Var var)
    Just (TFormula n) -> do
      args <- many (deltaList1 env)
      when (length args /= n) $
        fail $ "expected " ++ show n ++ " arguments, got " ++ show (length args)
      return (Lang.Formula var args)
    Nothing -> do
      setOffset o
      fail $ var ++ " is undefined"

variable :: Env -> Parser (ExprL EList)
variable env = do
  o <- getOffset
  var <- try varName
  case lookup var env of
    Just TList ->
      return (Var var)
    Just ty -> do
      setOffset o
      fail $ var ++ " expected to be a list, got " ++ show ty
    Nothing -> do
      setOffset o
      fail $ var ++ " is undefined"

deltaForall :: Env -> Parser (ExprL Bool)
deltaForall env = do
  var <- varName <* keyword "in"
  Forall var <$> deltaList env <* keyword "." <*> (deltaTerm $ (var, TList) : env)

deltaExists :: Env -> Parser (ExprL Bool)
deltaExists env = do
  var <- varName <* keyword "in"
  Exists var <$> deltaList env <* keyword "." <*> (deltaTerm $ (var, TList) : env)

deltaLet :: Env -> Parser (ExprL Bool)
deltaLet env = do
  var <- varName <* symbol "="
  Let var <$> deltaList env <* symbol "." <*> (deltaTerm $ (var, TList) : env)

deltaListFormula :: Env -> Parser (ExprL Bool)
deltaListFormula env =
  deltaListFormula' env =<< deltaList env

deltaListFormula' :: Env -> ExprL EList -> Parser (ExprL Bool)
deltaListFormula' env l = do
  choice
    [ LIn l <$ symbol "in" <*> deltaList env
    , LEq l <$ symbol "="  <*> deltaList env
    ]

deltaList1 :: Env -> Parser (ExprL EList)
deltaList1 env = choice
  [ variable env
  , LConst <$> listLit
  , parens (deltaList env)
  ]

deltaList :: Env -> Parser (ExprL EList)
deltaList env = choice
  [ parens (deltaList env)
  , LConst <$> listLit
  , LTail <$ keyword "tail" <*> (deltaList1 env)
  , LConc <$ keyword "conc" <*> (deltaList1 env) <*> (deltaList1 env)
  , LConst [] <$ keyword "nil"
  , variable env
  ] <?> "list"
