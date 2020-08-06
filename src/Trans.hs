{-# LANGUAGE GADTs #-}
module Trans where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as DMap

import Data.Set (Set, member)
import qualified Data.Set as DSet

import Delta.Lang (List)

import qualified Delta.Lang as Delta
import qualified QBF.Lang as QBF

type Matrix = Map Int [QBF.Expr]

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

data TranslationErr
  = UndefinedVar | Other
    deriving (Show, Eq)

convertListToMatrix :: Bounds -> List -> Either ConvertErr Matrix
convertListToMatrix bounds list
  | list == [] = Right $ createEmptyMatrix (praElements bounds) (maxListLength bounds)
  | otherwise = converter bounds list (createEmptyMatrix (praElements bounds) (maxListLength bounds)) 0
    where
      createEmptyMatrix praElems maxLength =
        DMap.fromList $ [(,) key (replicate maxLength (QBF.Const False)) | key <- DSet.toList praElems]

      converter :: Bounds -> List -> Matrix -> Int -> Either ConvertErr Matrix
      converter bounds (x:xs) matrix index
                | index > maxListLength bounds = Left $ MaxListLengthExceeded (x:xs) -- Bad
                | not $ member x (praElements bounds) = Left PraElementNotPresent
                | xs == [] = Right $ addValue x index matrix
                | otherwise = converter bounds xs (addValue x index matrix) (index + 1)
                where
                  addValue value position matrix = case DMap.lookup value matrix of
                    Just list -> DMap.insert value (take position list ++ [QBF.Const True] ++ drop (position + 1) list) matrix
                    Nothing -> undefined

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

translateToQBF :: Bounds -> Delta.Expr Matrix a -> Either TranslationErr QBF.Expr

translateToQBF bounds = undefined



evaluateMatrix :: Bounds -> Delta.Expr Matrix Delta.EList -> Either TranslationErr Matrix
evaluateMatrix = undefined
