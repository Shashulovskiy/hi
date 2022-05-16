{-# LANGUAGE QuasiQuotes #-}

module HITests.T8Spec where

import HITests.Core
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Literals" $ do
    it "literals" $ do
      "parse-time" =? Ok "parse-time"
      [r|parse-time("2021-12-15 00:00:00 UTC")|] =? Ok [r|parse-time("2021-12-15 00:00:00 UTC")|]
      [r|  parse-time( "2021-12-15 00:00:00 UTC" )  |] =? Ok [r|parse-time("2021-12-15 00:00:00 UTC")|]

      "now" =? Ok "now"

  describe "Functions" $ do
    it "Overloads" $ do
      [r|parse-time("2021-12-15 00:00:00 UTC") + 1000|] =? Ok [r|parse-time("2021-12-15 00:16:40 UTC")|]
      [r|parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC")|] =? Ok "3.351843755"
      [r|parse-time("2021-01-01 00:00:00 UTC") + 365 * 24 * 60 * 60|] =? Ok [r|parse-time("2022-01-01 00:00:00 UTC")|]