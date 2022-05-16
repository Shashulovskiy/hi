{-# LANGUAGE QuasiQuotes #-}

module HITests.T11Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Literals" $ do
    it "literals" $ do
      [r|{}|] =? Ok "{ }"
      [r|{ "width": 120, "height": 80 }|] =? Ok [r|{ "height": 80, "width": 120 }|]
      [r|{ 1: true, 3: true, 4: false }|] =? Ok [r|{ 1: true, 3: true, 4: false }|]

    it "functions" $ do
      "count" =? Ok "count"
      "keys" =? Ok "keys"
      "values" =? Ok "values"
      "invert" =? Ok "invert"

  describe "Functions" $ do
    it "Functions" $ do
      [r|keys({ "width": 120, "height": 80 })|] =? Ok [r|[ "height", "width" ]|]
      [r|values({ "width": 120, "height": 80 })|] =? Ok [r|[ 80, 120 ]|]
      [r|count("XXXOX")|] =? Ok [r|{ "O": 1, "X": 4 }|]
      [r|count([# 58 58 58 4f 58 #])|] =? Ok [r|{ 79: 1, 88: 4 }|]
      [r|count([true, true, false, true])|] =? Ok [r|{ false: 1, true: 3 }|]
      [r|invert({ "x": 1, "y" : 2, "z": 1 })|] =? Ok [r|{ 1: [ "z", "x" ], 2: [ "y" ] }|]

    it "Dot calls" $ do
      [r|{ "width": 120, "height": 80 }.width|] =? Ok "120"

  describe "Example session" $ do
    it "Session" $ do
      [r|count("Hello World").o|] =? Ok "2"
      [r|invert(count("big blue bag"))|] =? Ok [r|{ 1: [ "u", "l", "i", "e", "a" ], 2: [ "g", " " ], 3: [ "b" ] }|]
      [r|fold(add, values(count("Hello, World!")))|] =? Ok "13"

  describe "Corner cases" $ do
    it "From google doc" $ do
      "length.abc" =? Ok "3"
      "length.1bc" =? ParseError
      "length.a1c" =? Ok "3"
      "length.a1c-aaa-aa" =? Ok "10"
      " length. a1c-aaa-aa " =? ParseError

      "reverse.hello" =? Ok [r|"olleh"|]
      "reverse.to-upper.hello" =? EvalError HiErrorInvalidArgument

      "{}" =? Ok "{ }"
      
      [r|if(true, { "width" : 1 }, 1+1).width|] =? Ok "1"
      
      [r|{ "A" : 1 }.B|] =? Ok "null"
      
      [r|{ 1 + 2: 3 + 4, ["abacaba", 1 + div(1, 2)]: { "a": 15 } }|] =? Ok [r|{ 3: 7, [ "abacaba", 1.5 ]: { "a": 15 } }|]