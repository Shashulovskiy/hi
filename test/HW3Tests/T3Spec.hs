module HITests.T3Spec where

import HITests.Core
import Test.Hspec

spec :: Spec
spec = do
  describe "Example session" $ do
    it "Session" $ do
      "2 + 2" =? Ok "4"
      "2 + 2 * 3" =? Ok "8"
      "(2 + 2) * 3" =? Ok "12"
      "2 + 2 * 3 == (2 + 2) * 3" =? Ok "false"
      "10 == 2*5 && 143 == 11*13" =? Ok "true"
  describe "Operations" $ do
    it "Arithmetic" $ do
      "10 / 2" =? Ok "5"
      "10 / 0" =? EvalError HiErrorDivideByZero
      "10 * 2" =? Ok "20"
      "10 + 2" =? Ok "12"
      "10 - 2" =? Ok "8"
      
    it "Comparision" $ do
      "10 < 2" =? Ok "false"
      "10 < 10" =? Ok "false"
      "10 < 11" =? Ok "true"
      
      "10 > 2" =? Ok "true"
      "10 > 10" =? Ok "false"
      "10 > 11" =? Ok "false"
      
      "10 >= 2" =? Ok "true"
      "10 >= 10" =? Ok "true"
      "10 >= 11" =? Ok "false"
      
      "10 <= 11" =? Ok "true"
      "10 <= 10" =? Ok "true"
      "10 <= 1" =? Ok "false"
      
      "10 == 1" =? Ok "false"
      "10 == 10" =? Ok "true"
      
      "10 /= 10" =? Ok "false"
      "10 /= 1" =? Ok "true"
      
    it "And" $ do
      "false && false" =? Ok "false"
      "false && true" =? Ok "false"
      "true && false" =? Ok "false"
      "true && true" =? Ok "true"
      
    it "And" $ do
      "false || false" =? Ok "false"
      "false || true" =? Ok "true"
      "true || false" =? Ok "true"
      "true || true" =? Ok "true"

