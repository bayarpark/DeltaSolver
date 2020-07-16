{-# LANGUAGE GADTs, OverloadedStrings #-}
import Test.Hspec

import Data.Text (Text)
import Text.Megaparsec (parse, errorBundlePretty)

import Delta.Lang
import Delta.Parse (delta)
import Delta.Solve (Solution(..), solve)

main :: IO ()
main = hspec $ do
  describe "Parse simple expressions" $ do
    it "expr1" $ do
      d <- expectValidDelta "0 + 1 * 0"
      d `shouldBe` BOp BOr (BConst False) (BOp BAnd (BConst True) (BConst False))
    it "expr2" $ do
      d <- expectValidDelta "0 + x in <1 2 3>"
      d `shouldBe` BOp BOr (BConst False) (LIn (Var "x") (LConst [3, 2, 1]))
    it "expr3" $ do
      d <- expectValidDelta "exists z in y. conc z x = y"
      d `shouldBe` Exists "z" (Var "y") (LEq (LConc (Var "z") (Var "x")) (Var "y"))
    it "expr4" $ do
      d <- expectValidDelta "forall z in <1 2>. (1 + conc z <1> in <1 2 1> * 0)"
      d `shouldBe` Forall "z" (LConst [2, 1]) (BOp BOr (BConst True) (BOp BAnd (LIn (LConc (Var "z") (LConst [1])) (LConst [1, 2, 1])) (BConst False)))
  describe "Solves simple expression" $ do
    it "suffix" $ do
      formula <- expectValidDelta "let x = <2 3>. let y = <1 2 3>. exists z in y. conc z x = y"
      let expected = Let "x" (LConst [3,2]) $
                     Let "y" (LConst [3,2,1]) $
                     Exists "z" (Var "y") $
                     LEq (LConc (Var "z") (Var "x")) (Var "y")
      formula `shouldBe` expected
      case solve mempty formula of
        Right x -> x `shouldBe` FTrue [("z", [1])]
        Left e -> fail $ "solving failed: " ++ show e
    it "1-elem" $ do
      formula <- expectValidDelta "let x = <1>. (!(x = <>) * tail x = <>)"
      formula `shouldBe` Let "x" (LConst [1]) (BOp BAnd (BNot (LEq (Var "x") (LConst []))) (LEq (LTail (Var "x")) (LConst [])))
      solve mempty formula `shouldBe` Right (FTrue [])
    it "beg" $ do
      formula <- expectValidDelta "let y = <1 2>. exists x in y. (!(x = <>) * tail x = <>)"
      solve mempty formula `shouldBe` Right (FTrue [("x", [1])])

expectValidDelta :: Text -> IO (Expr Bool)
expectValidDelta d =
  case parse delta "" d of
    Right x -> return x
    Left e -> do
      putStr (errorBundlePretty e)
      fail "failed"
