{-# LANGUAGE GADTs, OverloadedStrings #-}
import Test.Hspec

import Data.Text (Text)
import Text.Megaparsec (parse, errorBundlePretty)

import Delta.Lang
import Delta.Parse (delta)

main :: IO ()
main = hspec $ do
  describe "Parse simple expressions" $ do
    it "expr1" $ do
      d <- expectValidDelta "0 + 1 * 0"
      d `shouldBe` BOp BOr (BConst False) (BOp BAnd (BConst True) (BConst False))
    it "expr2" $ do
      d <- expectValidDelta "0 + x in <1 2 3>"
      d `shouldBe` BOp BOr (BConst False) (LIn (Var "x") (LConst [1, 2, 3]))
    it "expr3" $ do
      d <- expectValidDelta "exists z in y. conc z x = y"
      d `shouldBe` Exists "z" (Var "y") (LEq (LConc (Var "z") (Var "x")) (Var "y"))

expectValidDelta :: Text -> IO (Expr Bool)
expectValidDelta d =
  case parse delta "" d of
    Right x -> return x
    Left e -> do
      putStr (errorBundlePretty e)
      fail "failed"
