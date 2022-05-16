{-# LANGUAGE QuasiQuotes #-}

module HITests.T6Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "ByteString" $ do
    it "literal" $ do
      "[##]" =? Ok "[# #]"
      "[#   #]" =? Ok "[# #]"
      "[# 01 3f ec #]" =? Ok "[# 01 3f ec #]"
      "[#  01  3f  ec  #]" =? Ok "[# 01 3f ec #]"

  describe "Functions" $ do
    it "Functions" $ do
      "pack-bytes([ 3, 255, 158, 32 ])" =? Ok "[# 03 ff 9e 20 #]"
      "unpack-bytes([# 10 20 30 #])" =? Ok "[ 16, 32, 48 ]"
      [r|encode-utf8("Hello!")|] =? Ok "[# 48 65 6c 6c 6f 21 #]"
      "decode-utf8([# 48 65 6c 6c 6f #])" =? Ok [r|"Hello"|]
      "decode-utf8([# c3 28 #])" =? Ok "null"
      "deserialise(serialise(1))" =? Ok "1"
      "deserialise(serialise([1, 2, 3]))" =? Ok "[ 1, 2, 3 ]"
      [r|deserialise(serialise("abc"))|] =? Ok [r|"abc"|]
      [r|deserialise(serialise([[], ["a", [# 00 ff #]]]))|] =? Ok [r|[ [ ], [ "a", [# 00 ff #] ] ]|]
      [r|deserialise(unzip(zip(serialise("abc"))))|] =? Ok [r|"abc"|]
      [r|deserialise(unzip(zip(serialise(1))))|] =? Ok "1"

    it "Overloads" $ do
      "[# 00 ff #] + [# 01 e3 #]" =? Ok "[# 00 ff 01 e3 #]"
      "[# 00 ff #] * 3" =? Ok "[# 00 ff 00 ff 00 ff #]"

  describe "Example sessions" $ do
    it "Session" $ do
      "pack-bytes(range(30, 40))" =? Ok "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
      [r|zip(encode-utf8("Hello, World!" * 2))|] =? Ok [r|[# 78 da f3 48 cd c9 c9 d7 51 08 cf 2f ca 49 51 f4 40 e6 00 00 78 91 08 d3 #]|]
      "decode-utf8([# 68 69 #] * 5)" =? Ok [r|"hihihihihi"|]
      "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" =? Ok "[# 01 02 03 #]"

  describe "Corner cases" $ do
    it "From google doc" $ do
      "[# 00 ff #](1)" =? Ok "255"
      "[# add(1,15) #]" =? ParseError

      "[# 01 23 #]" =? Ok "[# 01 23 #]"
      "[# 1 23 #]" =? ParseError
      "[# 0 1 2 3 #]" =? ParseError
      "[# (1 + 2) #]" =? ParseError

      "pack-bytes([256])" =? EvalError HiErrorInvalidArgument
      "pack-bytes([-1])" =? EvalError HiErrorInvalidArgument
      "pack-bytes([-1, 255, 15])" =? EvalError HiErrorInvalidArgument

      "[# #]" =? Ok "[# #]"
      "pack-bytes([])" =? Ok "[# #]"
      
      "[# 00 ff #](1)" =? Ok "255"
