module HITests.T2Spec where

import HITests.Core
import Test.Hspec

spec :: Spec
spec = do
  describe "Booleans" $ do
    it "Constants" $ do
      "true" =? Ok "true"
      "false" =? Ok "false"

    it "Boolean algebra" $ do
      "not(true)" =? Ok "false"
      "and(true, false)" =? Ok "false"
      "or(true, false)" =? Ok "true"

    it "Equality checking" $ do
      "equals(10, 10)" =? Ok "true"
      "equals(false, false)" =? Ok "true"
      "equals(3, 10)" =? Ok "false"
      "equals(1, true)" =? Ok "false"

    it "Comparisions" $ do
      "less-than(3, 10)" =? Ok "true"
      "less-than(false, true)" =? Ok "true"
      "less-than(false, 0)" =? Ok "true"
      "less-than(0, true)" =? Ok "false"

    it "Branching" $ do
      "if(true, 1, add)" =? Ok "1"
      "if(false, 1, add)" =? Ok "add"

  describe "Example session" $ do
    it "Session" $ do
      "false" =? Ok "false"
      "equals(add(2, 2), 4)" =? Ok "true"
      "less-than(mul(999, 99), 10000)" =? Ok "false"
      "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" =? Ok "-1"
      "and(less-than(0, 1), less-than(1, 0))" =? Ok "false"

  describe "Functions are values" $ do
    it "Functions are values" $ do
      "if(true, add, mul)" =? Ok "add"
      "if(true, add, mul)(10, 10)" =? Ok "20"
      "if(false, add, mul)(10, 10)" =? Ok "100"
    it "Function equality" $ do
      "equals(add, add)" =? Ok "true"
      "equals(add, mul)" =? Ok "false"
      
  describe "Corner cases" $ do
    it "From google doc" $ do
      "1 < 2 < 3" =? ParseError
      "1 > 2 > 3" =? ParseError
      "1 > 2 < 3" =? ParseError
      "1 < 2 <= 3" =? ParseError