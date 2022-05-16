{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HI.Instances where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

import HI.Base (HiValue (..))

class HasLength a => Sliceable a where
  slice :: Int -> Int -> a -> HiValue
  slice l r s =
    let lenS = len s
        lS = if l < 0 then 0 `max` (l + lenS) else l
        rS = if r < 0 then 0 `max` (r + lenS) else r
     in wrap . takeN (rS - lS) . dropN lS $ s

  takeN :: Int -> a -> a
  dropN :: Int -> a -> a

  wrap :: a -> HiValue

class HasLength a => Indexable a where
  at :: a -> Int -> HiValue
  at = atOrNull

  atOrNull :: a -> Int -> HiValue
  atOrNull s pos =
    if inBounds s pos
      then s `at` pos
      else HiValueNull

class HasLength a where
  len :: a -> Int

  inBounds :: a -> Int -> Bool
  inBounds s pos = pos >= 0 && pos < len s

instance Indexable Text where
  at :: Text -> Int -> HiValue
  at s pos = HiValueString $ T.pack [T.index s pos]

instance HasLength Text where
  len :: Text -> Int
  len = T.length

instance Sliceable Text where
  takeN :: Int -> Text -> Text
  takeN = T.take

  dropN :: Int -> Text -> Text
  dropN = T.drop

  wrap :: Text -> HiValue
  wrap = HiValueString

instance Indexable (Seq HiValue) where
  at :: Seq HiValue -> Int -> HiValue
  at s pos = fromMaybe HiValueNull (s S.!? pos)

instance HasLength (Seq a) where
  len :: Seq a -> Int
  len = S.length

instance Sliceable (Seq HiValue) where
  takeN :: Int -> Seq HiValue -> Seq HiValue
  takeN = S.take

  dropN :: Int -> Seq HiValue -> Seq HiValue
  dropN = S.drop

  wrap :: Seq HiValue -> HiValue
  wrap = HiValueList

instance Indexable ByteString where
  at :: ByteString -> Int -> HiValue
  at s pos = HiValueNumber $ toRational $ fromEnum (B.index s pos)

instance HasLength ByteString where
  len :: ByteString -> Int
  len = B.length

instance Sliceable ByteString where
  takeN :: Int -> ByteString -> ByteString
  takeN = B.take

  dropN :: Int -> ByteString -> ByteString
  dropN = B.drop

  wrap :: ByteString -> HiValue
  wrap = HiValueBytes
