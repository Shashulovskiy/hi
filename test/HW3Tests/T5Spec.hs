{-# LANGUAGE QuasiQuotes #-}

module HITests.T5Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Lists" $ do
    it "literal" $ do
      "[]" =? Ok "[ ]"
      "[ ]" =? Ok "[ ]"
      "[ 1 ]" =? Ok "[ 1 ]"
      "[ 1, 2, 3 ]" =? Ok "[ 1, 2, 3 ]"
      "[ 1, add, 3 ]" =? Ok "[ 1, add, 3 ]"
      "[ 1, add, 3 + 2 ]" =? Ok "[ 1, add, 5 ]"

  describe "Functions" $ do
    it "Functions" $ do
      "[ 1, add, 3 + 2 ]" =? Ok "[ 1, add, 5 ]"
      "range(5, 10.3)" =? Ok "[ 5, 6, 7, 8, 9, 10 ]"
      "fold(add, [11, 22, 33])" =? Ok "66"
      "fold(mul, [11, 22, 33])" =? Ok "7986"
      "fold(div, [11, 22, 33])" =? Ok "1/66"

    it "Overloads" $ do
      [r|length([1, true, "Hello"])|] =? Ok "3"
      [r|reverse([1, true, "Hello"])|] =? Ok [r|[ "Hello", true, 1 ]|]
      [r|[1, 2] + [3, 4, 5]|] =? Ok "[ 1, 2, 3, 4, 5 ]"
      [r|[0, "x"] * 3|] =? Ok [r|[ 0, "x", 0, "x", 0, "x" ]|]

  describe "Example session" $ do
    it "Session" $ do
      "list(1, 2, 3, 4, 5)" =? Ok "[ 1, 2, 3, 4, 5 ]"
      "fold(add, [2, 5] * 3)" =? Ok "21"
      "fold(mul, range(1, 10))" =? Ok "3628800"
      [r|[0, true, false, "hello", "world"](2, 4)|] =? Ok [r|[ false, "hello" ]|]
      "reverse(range(0.5, 70/8))" =? Ok "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"

  describe "Corner cases" $ do
    it "From google doc" $ do
      "[1 + 2, 3 + 4]" =? Ok "[ 3, 7 ]"

      "[]" =? Ok "[ ]"
      "list()" =? Ok "[ ]"

      "[1,2,3](0)" =? Ok "1"
