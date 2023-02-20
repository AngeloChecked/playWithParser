module DummySpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "" $ 
    it "" $
      5 `shouldBe` 5
