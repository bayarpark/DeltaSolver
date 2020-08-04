{-# LANGUAGE GADTs #-}
module Trans where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Delta.Lang (List)

import qualified Delta.Lang as Delta
import qualified QBF.Lang as QBF

type Matrix = Map Int [Bool]

data Bounds = Bounds
  { maxListLength :: Int
  , praElements   :: Set Int
  }

data ConvertErr
  -- | Found a list which's out of bounds of length limit
  = MaxListLengthExceeded List
  -- | Found an element in some list which's not present in set of praElements
  | PraElementNotPresent
  deriving (Show, Eq)

convertListToMatrix :: Bounds -> List -> Either ConvertErr Matrix
convertListToMatrix = undefined

convertListsToMatrices :: Bounds -> Delta.Expr List a -> Either ConvertErr (Delta.Expr Matrix a)
convertListsToMatrices bounds = \case
  Delta.LConst list -> Delta.LConst <$> convertListToMatrix bounds list

  Delta.LTail listExpr -> Delta.LTail <$> convertListsToMatrices bounds listExpr
  Delta.LConc listE1 listE2 -> Delta.LConc <$> convertListsToMatrices bounds listE1 <*> convertListsToMatrices bounds listE2

  Delta.LIn listE1 listE2 -> Delta.LIn <$> convertListsToMatrices bounds listE1 <*> convertListsToMatrices bounds listE2
  Delta.LEq listE1 listE2 -> Delta.LEq <$> convertListsToMatrices bounds listE1 <*> convertListsToMatrices bounds listE2

  Delta.Let varName listE boolE -> Delta.Let varName <$> convertListsToMatrices bounds listE <*> convertListsToMatrices bounds boolE
  Delta.Var varName -> Right $ Delta.Var varName

  Delta.Exists varName listE boolE -> Delta.Exists varName <$> convertListsToMatrices bounds listE <*> convertListsToMatrices bounds boolE
  Delta.Forall varName listE boolE -> Delta.Forall varName <$> convertListsToMatrices bounds listE <*> convertListsToMatrices bounds boolE

  Delta.Formula formulaName args -> Delta.Formula formulaName <$> traverse (convertListsToMatrices bounds) args
  Delta.BConst x -> Right $ Delta.BConst x
  Delta.BNot e -> Delta.BNot <$> convertListsToMatrices bounds e
  Delta.BOp op e1 e2 -> Delta.BOp op <$> convertListsToMatrices bounds e1 <*> convertListsToMatrices bounds e2
