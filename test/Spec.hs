{-# LANGUAGE GADTs, OverloadedStrings, TupleSections #-}
import Test.Hspec

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.Megaparsec (parse, errorBundlePretty)

import Delta.Lang hiding (Formula)
import Delta.Parse (delta, peekSignatures)
import Delta.Solve (Formulas, Solution(..), Formula(..), solveFormula)
import Trans (Bounds(..), convertListsToMatrices)
import qualified Delta.Lang as Lang
import qualified QBF.Lang as QBF

main :: IO ()
main = hspec $ do
  describe "Parse simple expressions" $ do
    it "expr1" $ do
      formulas <- validFormula "expr1 = 0 + 1 * 0;"
      Map.toList formulas `shouldBe`
        [ ("expr1",)
        $ Formula []
        $ BOp BOr (BConst False) (BOp BAnd (BConst True) (BConst False))
        ]
    it "expr2" $ do
      formulas <- validFormula "expr2 x = 0 + x in <1 2 3>"
      Map.toList formulas `shouldBe`
        [ ("expr2",)
        $ Formula ["x"]
        $ BOp BOr (BConst False)
                  (Var "x" `LIn` LConst [1,2,3])
        ]
    it "expr3" $ do
      formulas <- validFormula "expr3 x = forall y in <2 3 4>. exists z in y. conc z x = y"
      Map.toList formulas `shouldBe`
        [ ("expr3",)
        $ Formula ["x"]
        $ Forall "y" (LConst [2,3,4])
        $ Exists "z" (Var "y")
        $ LEq (LConc (Var "z") (Var "x")) (Var "y")
        ]
    it "expr4" $ do
      formulas <- validFormula "expr4 = forall z in <1 2>. (1 + conc z <1> in <1 2 1> * 0)"
      Map.toList formulas `shouldBe`
        [ ("expr4",)
        $ Formula []
        $ Forall "z" (LConst [1,2])
        $ BOp BOr (BConst True)
                  (BOp BAnd (LIn (LConc (Var "z") (LConst [1])) (LConst [1, 2, 1]))
                            (BConst False))
        ]
  describe "Solves simple expression" $ do
    it "suffix" $ do
      formulas <- validFormula "suffix x y = exists z in y. conc z x = y"
      Map.toList formulas `shouldBe`
        [ ("suffix",)
        $ Formula ["x", "y"]
        $ Exists "z" (Var "y")
        $ LConc (Var "z") (Var "x") `LEq` Var "y"
        ]
      solveFormula formulas "suffix" [[2,3], [1,2,3]] `shouldBe` Right (FTrue [("z", [1])])
      solveFormula formulas "suffix" [[1,2], [1,2,3]] `shouldBe` Right (FFalse [])
    it "oneElem" $ do
      formulas <- validFormula "oneElem x = !(x = <>) * tail x = <>"
      Map.toList formulas `shouldBe`
        [ ("oneElem",)
        $ Formula ["x"]
        $ BOp BAnd (BNot (LEq (Var "x") (LConst [])))
                   (LEq (LTail (Var "x")) (LConst []))
        ]
      solveFormula formulas "oneElem" [[1]] `shouldBe` Right (FTrue [])
      solveFormula formulas "oneElem" [[1, 2]] `shouldBe` Right (FFalse [])
    it "beg" $ do
      formulas <- validFormula $
        "oneElem x = !(x = <>) * tail x = <>;" <>
        "beg x y = x in y * oneElem x"
      solveFormula formulas "beg" [[1], [1, 2]] `shouldBe` Right (FTrue [])
    it "reverse" $ do
      formulas <- validFormula $
        "oneElem x = !(x = <>) * tail x = <>;" <>
        "beg x y = x in y * oneElem x;" <>
        "rev x y = (  x = nil + oneElem x  -> x = y)" <>
        "        * (!(x = nil + oneElem x) -> exists z in x. exists t in y. ( beg z x " <>
        "                                                                    * y = conc t z " <>
        "                                                                    * rev (tail x) t))"
      solveFormula formulas "rev" [[1,2,3], [3,2,1]] `shouldBe` Right (FTrue [("z",[1]),("t",[3,2]),("z",[2]),("t",[3])])
      solveFormula formulas "rev" [[1,2,3], [3,2,3]] `shouldBe` Right (FFalse [])
      solveFormula formulas "rev" [[1,2,3], [1,2,3]] `shouldBe` Right (FFalse [])
      solveFormula formulas "rev" [[1,2,3], []] `shouldBe` Right (FFalse [])
  describe "Converts list-based expressions to matrix-based" $ do
    it "expr1" $ do
      let l1 = [1,2]
          l2 = [1,2,3]
          m1 = Map.fromList [ (1, [QBF.Const True, QBF.Const False, QBF.Const False])
                            , (2, [QBF.Const False, QBF.Const True, QBF.Const False])
                            , (3, [QBF.Const False, QBF.Const False, QBF.Const False])
                            ]
          m2 = Map.fromList [ (1, [QBF.Const True,  QBF.Const False, QBF.Const False])
                            , (2, [QBF.Const False, QBF.Const True,  QBF.Const False])
                            , (3, [QBF.Const False, QBF.Const False, QBF.Const True])
                            ]
      let listExpr = LConst l1 `LIn` LConst l2 -- [1,2] in [1,2,3]
          matrixExpr = LConst m1 `LIn` LConst m2
          bounds = Bounds { maxListLength = 3, praElements = Set.fromList [1,2,3] }
      convertListsToMatrices bounds listExpr `shouldBe` Right matrixExpr


validFormula :: Text -> IO Formulas
validFormula d = do
  signatures <- case parse peekSignatures "" d of
    Right x -> return x
    Left e -> do
      putStr (errorBundlePretty e)
      fail "peeking signatures failed"
  case parse (delta signatures) "" d of
    Right x -> return x
    Left e -> do
      putStr (errorBundlePretty e)
      fail "parsing formula failed"
