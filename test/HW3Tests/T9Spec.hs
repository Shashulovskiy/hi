module HITests.T9Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Literals" $ do
    it "literals" $ do
      "rand" =? Ok "rand"
      "rand(0, 10)" =? Ok "rand(0, 10)"