{-# LANGUAGE OverloadedStrings #-}
module ShuntingYardMegaSpec where

import Test.Hspec 
import ShuntingYardMega ( parseShutingYard ) 
import Text.Megaparsec (eof,runParser)
import Expr

spec :: Spec
spec = 
    describe "shunting yard using mega parsec" $ do
        it "42" $
          runParser (parseShutingYard <* eof) "" "42" `shouldBe` Right (Lit 42)
        it "1 + 2" $
          runParser (parseShutingYard <* eof) "" "1 + 2" `shouldBe` Right (Add (Lit 1) (Lit 2))
