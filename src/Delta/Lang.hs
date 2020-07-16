{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving #-}
module Delta.Lang where

type List = [Int]

data Expr :: * -> * where
  BConst :: Bool -> Expr Bool
  BNot   :: Expr Bool -> Expr Bool
  BOp    :: BOp -> Expr Bool -> Expr Bool -> Expr Bool

  LConst :: List -> Expr List
  LTail  :: Expr List -> Expr List
  LConc  :: Expr List -> Expr List -> Expr List

  LIn :: Expr List -> Expr List -> Expr Bool
  LEq :: Expr List -> Expr List -> Expr Bool

  Let :: String -> Expr List -> Expr Bool -> Expr Bool
  Var :: String -> Expr List

  Exists :: String -> Expr List -> Expr Bool -> Expr Bool
  Forall :: String -> Expr List -> Expr Bool -> Expr Bool

deriving instance Show e => Show (Expr e)
deriving instance Eq e => Eq (Expr e)

data BOp
  = BAnd
  | BOr
  deriving (Show, Eq)
