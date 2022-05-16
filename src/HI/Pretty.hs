{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HI.Pretty
  ( prettyParseError
  , prettyResult
  , prettyValue
  ) where

import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Scientific (FPFormat (..), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Sequence as S
import Data.Void (Void)
import GHC.Real (Ratio (..))
import GHC.Word (Word8)
import Prettyprinter (annotate, encloseSep, pretty, viaShow, (<+>))
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal.Internal (AnsiStyle, Color (..), bold, color)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.Printf (printf)

import HI.Base (HiAction (..), HiError, HiValue (..))

-- | Helper function to pretty HiError or HiValue
prettyResult :: Either HiError HiValue -> Doc AnsiStyle
prettyResult (Left e)  = prettyError e
prettyResult (Right v) = prettyValue v

-- | Pretty HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n@(a :% b)) = case b of
  1 -> pretty $ show a
  _ ->
    let (number, repetend) = fromRationalRepetendUnlimited n
     in case repetend of
          Nothing -> pretty $ formatScientific Fixed Nothing number
          Just _  ->
            let (q, r) = quotRem a b
                sign = if q < 0 then " - " else " + "
                wholePart = if q == 0 then "" else show q <> sign
                resultEnumerator = if q == 0 then r else abs r
                fractionalPart = show resultEnumerator <> "/" <> show b
             in pretty $ wholePart <> fractionalPart
prettyValue (HiValueBool a) = pretty $ map toLower $ show a
prettyValue (HiValueString a) = viaShow a
prettyValue (HiValueFunction a) = viaShow a
prettyValue (HiValueList a) =
  if S.null a
    then "[ ]"
    else encloseSep "[ " " ]" ", " (prettyValue <$> toList a)
prettyValue (HiValueBytes a) =
  if B.null a
    then "[# #]"
    else encloseSep "[# " " #]" " " $ pretty . printf @(Word8 -> String) "%02x" <$> B.unpack a
prettyValue (HiValueAction a) = case a of
  HiActionRead p    -> "read(" <> viaShow p <> ")"
  HiActionWrite p w -> "write(" <> viaShow p <> "," <+> prettyValue (HiValueBytes w) <> ")"
  HiActionMkDir p   -> "mkdir(" <> viaShow p <> ")"
  HiActionChDir p   -> "cd(" <> viaShow p <> ")"
  HiActionCwd       -> "cwd"
  HiActionNow       -> "now"
  HiActionRand l r  -> "rand(" <> viaShow l <> "," <+> viaShow r <> ")"
  HiActionEcho t    -> "echo(" <> viaShow t <> ")"
prettyValue (HiValueTime a) = "parse-time(\"" <> viaShow a <> "\")"
prettyValue (HiValueDict a) =
  if M.null a
    then "{ }"
    else encloseSep "{ " " }" ", " ((\(k, v) -> prettyValue k <> ":" <+> prettyValue v) <$> M.assocs a)
prettyValue HiValueNull = "null"

-- | Pretty HiError
prettyError :: HiError -> Doc AnsiStyle
prettyError e = annotate (color Red <> bold) (pretty $ show e)

-- | Pretty ParseErrorBundle
prettyParseError :: ParseErrorBundle String Void -> Doc AnsiStyle
prettyParseError = pretty . errorBundlePretty
