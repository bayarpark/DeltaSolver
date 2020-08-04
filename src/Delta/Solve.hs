{-# LANGUAGE GADTs #-}
module Delta.Solve where

import Data.Function (fix)
import Data.List (isPrefixOf, inits)
import Data.Map (Map)
import qualified Data.Map as M

import Delta.Lang
import qualified Delta.Lang as Lang

type Formulas = Map String Formula
data Formula = Formula
  { argNames :: [String]
  , expr     :: ExprL Bool
  } deriving (Show, Eq)

type Vars = [(String, List)]

data Solution
  = FTrue  Vars
  | FFalse Vars
  deriving (Show, Eq)

data SolveErr
  = UndefinedVar
  | UndefinedFormula
  | FormulaArgumentsMismatch
  deriving (Eq, Show)

solveFormula :: Formulas -> String -> [List] -> Either SolveErr Solution
solveFormula formulas fName args =
  solve formulas [] (Lang.Formula fName $ LConst <$> args)

solve :: Formulas -> Vars -> ExprL Bool -> Either SolveErr Solution
solve formulas env = \case
  BConst True -> Right (FTrue [])
  BConst False -> Right (FFalse [])
  BNot x -> solve formulas env x >>= \case
    FTrue e -> Right $ FFalse e
    FFalse e -> Right $ FTrue e
  BOp BAnd x y -> solve formulas env x >>= \case
    FFalse e -> Right $ FFalse e
    FTrue e1 -> solve formulas env y >>= \case
      FFalse e2 -> Right $ FFalse e2
      FTrue e2 -> Right $ FTrue (e1 ++ e2)
  BOp BOr x y -> solve formulas env x >>= \case
    FTrue e -> Right $ FTrue e
    FFalse e1 -> solve formulas env y >>= \case
      FTrue e2 -> Right $ FTrue e2
      FFalse e2 -> Right $ FFalse (e1 ++ e2)

  LIn prefix list -> do
    p <- evaluate env prefix
    l <- evaluate env list
    if p `isPrefixOf` l
      then Right $ FTrue []
      else Right $ FFalse []
  LEq l1 l2 -> do
    p1 <- evaluate env l1
    p2 <- evaluate env l2
    if p1 == p2
      then Right $ FTrue []
      else Right $ FFalse []
  Let var list expr -> do
    l <- evaluate env list
    solve formulas ((var, l) : env) expr

  Exists var boundList' formula -> do
    boundList <- evaluate env boundList'
    flip fix (inits boundList) $ \repeat -> \case
      [] -> Right $ FFalse []
      boundList : nextBoundList -> do
        let env' = (var, boundList) : env
        solve formulas env' formula >>= \case
          FTrue e -> Right $ FTrue $ (var, boundList) : e
          FFalse e -> repeat nextBoundList
  Forall var boundList' formula -> do
    boundList <- evaluate env boundList'
    flip fix (inits boundList) $ \repeat -> \case
      [] -> Right $ FTrue []
      boundList : nextBoundList -> do
        let env' = (var, boundList) : env
        solve formulas env' formula >>= \case
          FFalse e -> Right $ FFalse $ (var, boundList) : e
          FTrue e -> repeat nextBoundList
  Lang.Formula fName args' ->
    case M.lookup fName formulas of
      Just f | length (argNames f) == length args' -> do
        args <- sequenceA $ evaluate env <$> args'
        solve formulas (argNames f `zip` args) (expr f)
      Just _  -> Left FormulaArgumentsMismatch
      Nothing -> Left UndefinedFormula

evaluate :: Vars -> ExprL EList -> Either SolveErr List
evaluate env = \case
  LConst l -> Right l
  LTail l -> evaluate env l >>= Right . \case
    _:xs -> xs
    [] -> []
  LConc a b -> (++) <$> evaluate env a <*> evaluate env b
  Var name -> case lookup name env of
    Just l -> Right l
    Nothing -> Left UndefinedVar
