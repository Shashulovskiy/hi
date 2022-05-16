module HITests.T1Spec where

import HITests.Core
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic functions" $ do
    it "No composition" $ do
      "mul(2, 10)" =? Ok "20"
      "sub(1000, 7)" =? Ok "993"
      "div(3, 5)" =? Ok "0.6"
      "add(500, 12)" =? Ok "512"
      "sub(10, 100)" =? Ok "-90"
      "mul(23, 768)" =? Ok "17664"
      "div(57, 190)" =? Ok "0.3"
    it "Composition" $ do
       "div(add(mul(2, 5), 1), sub(11,6))" =? Ok "2.2"
  describe "Errors" $ do
     it "Arity missmatch" $ do
       "sub(1)" =? EvalError HiErrorArityMismatch
       "sub(1, 2, 3)" =? EvalError HiErrorArityMismatch
     it "Division by zero" $ do
       "div(1, 0)" =? EvalError HiErrorDivideByZero
       "div(1, sub(5, 5))" =? EvalError HiErrorDivideByZero
     it "Invalid function" $ do
       "15(2)" =? EvalError HiErrorInvalidFunction
     it "Invalid argument" $ do
       "sub(10, add)" =? EvalError HiErrorInvalidArgument
  describe "Pretty" $ do
     it "Integers" $ do
       "42" =? Ok "42"
       "-8" =? Ok "-8"
       "15" =? Ok "15"
     it "Finite decimal fractions" $ do
       "3.14" =? Ok "3.14"
       "-8.15" =? Ok "-8.15"
       "77.01" =? Ok "77.01"
     it "Fractions" $ do
       "div(1, 3)" =? Ok "1/3"
       "sub(0, div(1, 7))" =? Ok "-1/7"
       "div(3, 11)" =? Ok "3/11"
     it "Mixed fractions" $ do
       "add(5, div(1, 3))" =? Ok "5 + 1/3"
       "sub(0, add(10, div(1, 7)))" =? Ok "-10 - 1/7"
       "add(24, div(3, 11))" =? Ok "24 + 3/11"
  describe "Example session" $ do
     it "Example session" $ do
       "100" =? Ok "100"
       "-15" =? Ok "-15"
       "add(100, -15)" =? Ok "85"
       "add(3, div(14, 100))" =? Ok "3.14"
       "div(10, 3)" =? Ok "3 + 1/3"
       "sub(mul(201, 11), 0.33)" =? Ok "2210.67"
  describe "Spaces" $ do
     it "No spaces" $ do
       "1" =? Ok "1"
       "3.14" =? Ok "3.14"
       "add(1,2)" =? Ok "3"
     it "Spaces" $ do
       "  1  " =? Ok "1"
       "  3.14  " =? Ok "3.14"
       "  -  1.2  " =? Ok "-1.2"
       "  add  (  1  ,  2  )  " =? Ok "3"
       "  add  (  add  (  1  ,  2  )  ,  2  )  " =? Ok "5"
  describe "Parsing" $ do
     it "Numbers" $ do
       "1" =? Ok "1"
       "3.14" =? Ok "3.14"
       "-3.14" =? Ok "-3.14"
     it "Expressions" $ do
       "add(1, 2)" =? Ok "3"
       "(add)(1, 2)" =? Ok "3"
       "((add))(1, 2)" =? Ok "3"
       "((add))(((add))(1, 2), 2)" =? Ok "5"

  describe "Corner cases" $ do
    it "From google doc" $ do
      "(div(1))(10)" =? EvalError HiErrorArityMismatch
      "(div)(1,2)" =? Ok "0.5"
