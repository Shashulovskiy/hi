{-# LANGUAGE QuasiQuotes #-}

module HITests.T4Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Strings" $ do
    it "literal" $ do
      [r|""|] =? Ok [r|""|]
      [r|"abacaba"|] =? Ok [r|"abacaba"|]
      [r|"  aca  "|] =? Ok [r|"  aca  "|]

  describe "Functions" $ do
    it "Functions" $ do
      [r|length("Hello World")|] =? Ok "11"
      [r|to-upper("Hello World")|] =? Ok [r|"HELLO WORLD"|]
      [r|to-lower("Hello World")|] =? Ok [r|"hello world"|]
      [r|reverse("stressed")|] =? Ok [r|"desserts"|]
      [r|trim(" Hello World ")|] =? Ok [r|"Hello World"|]

    it "Operator overloads" $ do
      [r|"Hello" + "World"|] =? Ok [r|"HelloWorld"|]
      [r|"/home/user" / "hi"|] =? Ok [r|"/home/user/hi"|]

    it "Indexing" $ do
      [r|"Hello World"(0)|] =? Ok [r|"H"|]
      [r|"Hello World"(7)|] =? Ok [r|"o"|]

    it "Indexing out of bounds" $ do
      [r|"Hello World"(-1)|] =? Ok "null"
      [r|"Hello World"(99)|] =? Ok "null"

    it "Slicing" $ do
      [r|"Hello World"(0, 5)|] =? Ok [r|"Hello"|]
      [r|"Hello World"(2, 4)|] =? Ok [r|"ll"|]

    it "Advanced slicing" $ do
      [r|"Hello World"(0, -4)|] =? Ok [r|"Hello W"|]
      [r|"Hello World"(-4, -1)|] =? Ok [r|"orl"|]
      [r|"Hello, World"(2, null)|] =? Ok [r|"llo, World"|]
      [r|"Hello, World"(null, 5)|] =? Ok [r|"Hello"|]

  describe "Example session" $ do
    it "Session" $ do
      [r|to-upper("what a nice language")(7, 11)|] =? Ok [r|"NICE"|]
      [r|"Hello" == "World"|] =? Ok "false"
      [r|length("Hello" + "World")|] =? Ok "10"
      [r|length("hehe" * 5) / 3|] =? Ok "6 + 2/3"

  describe "Corner cases" $ do
    it "From google doc" $ do
      "length.hello-world" =? Ok "11"
      [r|"abc"(1, 2.2)|] =? EvalError HiErrorInvalidArgument

      [r|"cat" * 0|] =? EvalError HiErrorInvalidArgument
      [r|"cat" * -1|] =? EvalError HiErrorInvalidArgument

      [r|"suicide"(4,100)|] =? Ok [r|"ide"|]

      [r|"Cat" * 5.2|] =? EvalError HiErrorInvalidArgument
      [r|"Cat" * 5.9|] =? EvalError HiErrorInvalidArgument




