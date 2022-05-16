{-# LANGUAGE QuasiQuotes #-}

module HITests.T10Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Literals" $ do
    it "literals" $ do
      "echo" =? Ok "echo"

  describe "Functions" $ do
    it "Lazy" $ do
      [r|"Hello"(0) || "Z"|] =? Ok [r|"H"|]
      [r|"Hello"(99) || "Z"|] =? Ok [r|"Z"|]

  describe "Corner cases" $ do
    it "From google doc" $ do
      "if(true, 1, 1/0)" =? Ok "1"
      "or(true, 3)" =? Ok "true"