{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Delta.Lang where

import Data.Void (Void)

type List = [Int]
type ExprL a = Expr List a

data EList = EList Void deriving (Eq, Show)

data Expr list a where
  BConst :: Bool -> Expr list Bool
  BNot   :: Expr list Bool -> Expr list Bool
  BOp    :: BOp -> Expr list Bool -> Expr list Bool -> Expr list Bool

  LConst :: list -> Expr list EList
  LTail  :: Expr list EList -> Expr list EList
  LConc  :: Expr list EList -> Expr list EList -> Expr list EList

  LIn :: Expr list EList -> Expr list EList -> Expr list Bool
  LEq :: Expr list EList -> Expr list EList -> Expr list Bool
  LLess :: Expr list EList -> Expr list EList -> Expr list Bool

  Let :: String -> Expr list EList -> Expr list Bool -> Expr list Bool
  Var :: String -> Expr list EList

  Exists :: String -> Expr list EList -> Expr list Bool -> Expr list Bool
  Forall :: String -> Expr list EList -> Expr list Bool -> Expr list Bool

  Formula :: String -> [Expr list EList] -> Expr list Bool

deriving instance (Show list, Show e) => Show (Expr list e)
deriving instance (Eq list, Eq e) => Eq (Expr list e)

data BOp
  = BAnd
  | BOr
  deriving (Show, Eq)
