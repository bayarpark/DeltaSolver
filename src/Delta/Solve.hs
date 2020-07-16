{-# LANGUAGE GADTs #-}
module Delta.Solve where

import Data.Function (fix)
import Data.List (isSuffixOf)

import Delta.Lang

type Env = [(String, List)]

data Solution
  = FTrue  [(String, List)]
  | FFalse [(String, List)]
  deriving (Show, Eq)

data SolveErr
  = UndefinedVar
  deriving (Eq, Show)

solve :: Env -> Expr Bool -> Either SolveErr Solution
solve env = \case
  BConst True -> Right (FTrue [])
  BConst False -> Right (FFalse [])
  BNot x -> solve env x >>= \case 
    FTrue e -> Right $ FFalse e
    FFalse e -> Right $ FTrue e
  BOp BAnd x y -> solve env x >>= \case
    FFalse e -> Right $ FFalse e
    FTrue e1 -> solve env y >>= \case
      FFalse e2 -> Right $ FFalse e2
      FTrue e2 -> Right $ FTrue (e1 ++ e2)
  BOp BOr x y -> solve env x >>= \case
    FTrue e -> Right $ FTrue e
    FFalse e1 -> solve env y >>= \case
      FTrue e2 -> Right $ FTrue e2
      FFalse e2 -> Right $ FFalse (e1 ++ e2)

  LIn prefix list -> do
    p <- evaluate env prefix
    l <- evaluate env list
    if p `isSuffixOf` l
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
    solve ((var, l) : env) expr

  Exists var boundList' formula -> do
    boundList <- evaluate env boundList'
    flip fix boundList $ \repeat boundList -> do
      let env' = (var, boundList) : env
      solve env' formula >>= \case
        FTrue e -> Right $ FTrue $ (var, boundList) : e
        FFalse e -> case boundList of
          _:xs -> repeat xs
          [] -> Right $ FFalse []
  Forall var boundList' formula -> do
    boundList <- evaluate env boundList'
    flip fix boundList $ \repeat boundList -> do
      let env' = (var, boundList) : env
      solve env' formula >>= \case
        FFalse e -> Right $ FFalse $ (var, boundList) : e
        FTrue e -> case boundList of
          _:xs -> repeat xs
          [] -> Right $ FTrue []

evaluate :: Env -> Expr List -> Either SolveErr List
evaluate env = \case
  LConst l -> Right l
  LTail l -> evaluate env l >>= Right . \case
    [] -> []
    _:xs -> xs
  LConc a b -> (++) <$> evaluate env b <*> evaluate env a
  Var name -> case lookup name env of
    Just l -> Right l
    Nothing -> Left UndefinedVar
