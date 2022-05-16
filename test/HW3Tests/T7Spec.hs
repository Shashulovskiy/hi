{-# LANGUAGE QuasiQuotes #-}

module HITests.T7Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Literals" $ do
    it "Non-evaluated" $ do
      "read" =? Ok "read"
      "write" =? Ok "write"
      "mkdir" =? Ok "mkdir"
      "cd" =? Ok "cd"
      "cwd" =? Ok "cwd"
    it "Non-evaluated calls" $ do
      [r|read("hi.txt")|] =? Ok [r|read("hi.txt")|]
      [r|write("hi.txt", "Hi!")|] =? Ok [r|write("hi.txt", [# 48 69 21 #])|]
      [r|mkdir("dir")|] =? Ok [r|mkdir("dir")|]
      [r|cd("dir")|] =? Ok [r|cd("dir")|]