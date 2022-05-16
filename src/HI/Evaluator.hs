{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HI.Evaluator
  ( eval
  ) where

import qualified Codec.Compression.Zlib as CMP
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either.Combinators (fromRight)
import Data.Foldable (toList)
import Data.Map (Map, fromListWith, mapKeys)
import qualified Data.Map as M
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import GHC.Real (Ratio (..))
import Text.Read (readMaybe)

import HI.Base (HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                 HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import HI.Instances (HasLength (len), Indexable (atOrNull), Sliceable (slice))

-- | Evaluates an expression. Returns HiValue on success
-- or HiError if evaluation fails
eval
  :: HiMonad m                  -- ^ HiMonad context to perform IO actions with permissions
  => HiExpr                     -- ^ Expression to be evaluated
  -> m (Either HiError HiValue) -- ^ Result of evaluation
eval e = runExceptT $ eval' e

-- | Helper function to run actions
run
  :: (HiMonad m)
  => HiAction            -- ^ Action to be run
  -> ExceptT e m HiValue -- ^ Result of running action
run = ExceptT . fmap Right . runAction

-- | Helper function for eval. Defined to use ExceptT monad transformer
-- to propagate HiError through the evaluator.
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue v) = return v
eval' (HiExprRun expr) = do
  e <- eval' expr
  case e of
    (HiValueAction action) -> run action
    _                      -> throwE HiErrorInvalidArgument
eval' (HiExprApply func args) = do
  evaluatedFunction <- eval' func -- Evaluate the function first
  if allowShortCircuit evaluatedFunction -- Check if lazy evaluation is possible
    then evalShortCircuit evaluatedFunction args
    else do
      -- Lazy evaluation not possible, evaluate args first
      evaluatedArgs <- sequenceA $ eval' <$> args
      evalWithArgs evaluatedFunction evaluatedArgs
eval' (HiExprDict entries) = do
  let (keys, values) = unzip entries
  evaluatedKeys <- sequenceA $ eval' <$> keys
  evaluatedValues <- sequenceA $ eval' <$> values
  return $ HiValueDict $ M.fromList (zip evaluatedKeys evaluatedValues)

-- | Helper function for performing evaluation.
evalWithArgs
  :: HiMonad m
  => HiValue                   -- ^ Pre-evaluated function
  -> [HiValue]                 -- ^ Pre-evaluated arguments
  -> ExceptT HiError m HiValue -- ^ Result of evaluation
evalWithArgs (HiValueFunction f) args =
  if (getArity f == length args) || (getArity f == -1)
    then apply f args
    else throwE HiErrorArityMismatch
evalWithArgs (HiValueString s) args = hiFunListOperation s args
evalWithArgs (HiValueList l) args = hiFunListOperation l args
evalWithArgs (HiValueBytes b) args = hiFunListOperation b args
evalWithArgs (HiValueDict d) args = hiFunDictLookUp d args
evalWithArgs _ _ = throwE HiErrorInvalidFunction

-- | Evaluate function lazily (without evaluating all of the arguments).
-- Should only be used on allowed functions. Checked via allowShortCircuit
evalShortCircuit
  :: (HiMonad m)
  => HiValue                   -- ^ Function that supports lazy evaluation
  -> [HiExpr]                  -- ^ Un-evaluated arguments
  -> ExceptT HiError m HiValue -- ^ The result of evaluation
evalShortCircuit (HiValueFunction HiFunIf) [cond, a, b] = do
  evaluatedCondition <- eval' cond
  case evaluatedCondition of
    (HiValueBool True)  -> eval' a
    (HiValueBool False) -> eval' b
    _                   -> throwE HiErrorInvalidArgument
evalShortCircuit (HiValueFunction HiFunAnd) [a, b] = do
  evaluatedA <- eval' a
  case evaluatedA of
    (HiValueBool False) -> return evaluatedA
    HiValueNull         -> return evaluatedA
    _                   -> eval' b
evalShortCircuit (HiValueFunction HiFunOr) [a, b] = do
  evaluatedA <- eval' a
  case evaluatedA of
    (HiValueBool False) -> eval' b
    HiValueNull         -> eval' b
    _                   -> return evaluatedA
evalShortCircuit _ _ = throwE HiErrorArityMismatch

-- | Apply function to pre-evaluated arguments.
-- Implement via pattern-matching on expected argument types.
-- Arity should be checked before-hand.
apply
  :: HiMonad m
  => HiFun                     -- ^ Function
  -> [HiValue]                 -- ^ Pre-evaluated arguments
  -> ExceptT HiError m HiValue -- ^ Result of evaluation
apply HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a + b
apply HiFunAdd [HiValueString a, HiValueString b] = return $ HiValueString $ a <> b
apply HiFunAdd [HiValueList a, HiValueList b] = return $ HiValueList $ a <> b
apply HiFunAdd [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes $ a <> b
apply HiFunAdd [HiValueTime t, HiValueNumber delta] = return $ HiValueTime $ addUTCTime (realToFrac delta) t
apply HiFunAdd [HiValueNumber delta, HiValueTime t] = return $ HiValueTime $ addUTCTime (realToFrac delta) t
apply HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a - b
apply HiFunSub [HiValueTime a, HiValueTime b] = return $ HiValueNumber . toRational $ diffUTCTime a b
apply HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a * b
apply HiFunMul [HiValueList a, HiValueNumber (n :% 1)] = HiValueList <$> hiFunListStimes n a
apply HiFunMul [HiValueNumber (n :% 1), HiValueList a] = HiValueList <$> hiFunListStimes n a
apply HiFunMul [HiValueString a, HiValueNumber (n :% 1)] = HiValueString <$> hiFunListStimes n a
apply HiFunMul [HiValueBytes a, HiValueNumber (n :% 1)] = HiValueBytes <$> hiFunListStimes n a
apply HiFunMul [HiValueNumber (n :% 1), HiValueString a] = HiValueString <$> hiFunListStimes n a
apply HiFunMul [HiValueNumber (n :% 1), HiValueBytes a] = HiValueBytes <$> hiFunListStimes n a
apply HiFunDiv [HiValueNumber _, HiValueNumber 0] = throwE HiErrorDivideByZero
apply HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a / b
apply HiFunDiv [HiValueString a, HiValueString b] = return $ HiValueString $ a <> "/" <> b
apply HiFunNot [HiValueBool a] = return $ HiValueBool $ not a
apply HiFunAnd [HiValueNull, _] = return HiValueNull
apply HiFunAnd [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a && b
apply HiFunOr [HiValueNull, b] = return b
apply HiFunOr [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a || b
apply HiFunLessThan [a, b] = return $ HiValueBool $ a < b
apply HiFunGreaterThan [a, b] = return $ HiValueBool $ a > b
apply HiFunEquals [a, b] = return $ HiValueBool $ a == b
apply HiFunNotEquals [a, b] = return $ HiValueBool $ a /= b
apply HiFunNotLessThan [a, b] = return $ HiValueBool $ a >= b
apply HiFunNotGreaterThan [a, b] = return $ HiValueBool $ a <= b
apply HiFunIf [HiValueBool True, a, _] = return a
apply HiFunIf [HiValueBool False, _, b] = return b
apply HiFunLength [HiValueString s] = return $ HiValueNumber $ toRational $ T.length s
apply HiFunLength [HiValueList l] = return $ HiValueNumber $ toRational $ S.length l
apply HiFunLength [HiValueBytes b] = return $ HiValueNumber $ toRational $ B.length b
apply HiFunToLower [HiValueString s] = return $ HiValueString $ T.toLower s
apply HiFunToUpper [HiValueString s] = return $ HiValueString $ T.toUpper s
apply HiFunReverse [HiValueString s] = return $ HiValueString $ T.reverse s
apply HiFunReverse [HiValueList s] = return $ HiValueList $ S.reverse s
apply HiFunReverse [HiValueBytes s] = return $ HiValueBytes $ B.reverse s
apply HiFunTrim [HiValueString s] = return $ HiValueString $ T.strip s
apply HiFunList l = return $ HiValueList $ S.fromList l
apply HiFunRange [HiValueNumber l, HiValueNumber r] = return $ HiValueList $ S.fromList $ HiValueNumber <$> [l .. r]
apply HiFunFold [f, HiValueList (h S.:<| t)] = foldM (\a b -> evalWithArgs f [a, b]) h t
apply HiFunFold [HiValueFunction _, HiValueList _] = return HiValueNull
apply HiFunPackBytes [HiValueList l] = do
  bytes <- sequenceA $ toByte <$> toList l
  return $ HiValueBytes $ B.pack bytes
apply HiFunUnpackBytes [HiValueBytes b] = return $ HiValueList $ S.fromList $ HiValueNumber . toRational <$> B.unpack b
apply HiFunEncodeUtf8 [HiValueString s] = return $ HiValueBytes $ encodeUtf8 s
apply HiFunDecodeUtf8 [HiValueBytes s] = return $ either (const HiValueNull) HiValueString (decodeUtf8' s)
apply HiFunZip [HiValueBytes s] = return $ HiValueBytes $ toStrict $ CMP.compressWith CMP.defaultCompressParams {CMP.compressLevel = CMP.bestCompression} (fromStrict s)
apply HiFunUnzip [HiValueBytes s] = return $ HiValueBytes $ toStrict $ CMP.decompressWith CMP.defaultDecompressParams (fromStrict s)
apply HiFunSerialise [a] = return $ HiValueBytes $ toStrict $ serialise a
apply HiFunDeserialise [HiValueBytes b] = return $ fromRight HiValueNull (deserialiseOrFail (fromStrict b))
apply HiFunRead [HiValueString path] = return $ HiValueAction $ HiActionRead (T.unpack path)
apply HiFunWrite [HiValueString path, HiValueString content] = return $ HiValueAction $ HiActionWrite (T.unpack path) (encodeUtf8 content)
apply HiFunWrite [HiValueString path, HiValueBytes bytes] = return $ HiValueAction $ HiActionWrite (T.unpack path) bytes
apply HiFunMkDir [HiValueString path] = return $ HiValueAction $ HiActionMkDir (T.unpack path)
apply HiFunChDir [HiValueString path] = return $ HiValueAction $ HiActionChDir (T.unpack path)
apply HiFunParseTime [HiValueString time] = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack time))
apply HiFunRand [HiValueNumber (l :% 1), HiValueNumber (r :% 1)] = return $ HiValueAction $ HiActionRand (fromIntegral l) (fromIntegral r)
apply HiFunEcho [HiValueString s] = return $ HiValueAction $ HiActionEcho s
apply HiFunKeys [HiValueDict d] = return $ HiValueList $ S.fromList $ M.keys d
apply HiFunValues [HiValueDict d] = return $ HiValueList $ S.fromList $ M.elems d
apply HiFunCount [HiValueString s] = return $ HiValueDict $ mapKeys (HiValueString . T.pack . (: [])) $ hiValueCount (T.unpack s)
apply HiFunCount [HiValueList l] = return $ HiValueDict $ hiValueCount (toList l)
apply HiFunCount [HiValueBytes b] = return $ HiValueDict $ mapKeys (HiValueNumber . toRational . fromEnum) $ hiValueCount (B.unpack b)
apply HiFunInvert [HiValueDict d] = return $ HiValueDict $ M.map (HiValueList . S.fromList) (hiValueInvert d)
apply _ _ = throwE HiErrorInvalidArgument

-- | Helper function to perform count operation on any list.
-- Resulting counter is wrapped in HiValue for convenience.
hiValueCount
  :: Ord a           -- ^ Constraint to allow using a as map key
  => [a]             -- ^ List that needs to be counted
  -> Map a HiValue   -- ^ Map with counted elements.
hiValueCount s = M.map HiValueNumber ((fromListWith (+) . map (,1)) s)

-- | Helper function to perform count operation on any map.
hiValueInvert
  :: Ord v
  => Map k v   -- ^ Original map
  -> Map v [k] -- ^ Inverted map
hiValueInvert m = M.fromListWith (++) pairs
  where
    pairs = [(v, [k]) | (k, v) <- M.toList m]

-- | Helper function to convert to bytes.
-- Expecting a whole HiValueNumber in range [0..255]
toByte
  :: HiMonad m
  => HiValue                   -- ^ Number to be converted
  -> ExceptT HiError m Word8   -- ^ The resulting byte
toByte (HiValueNumber (n :% 1)) =
  if n >= 0 && n <= 255
    then return $ toEnum (fromIntegral n)
    else throwE HiErrorInvalidArgument
toByte _ = throwE HiErrorInvalidArgument

-- | Helper function to perform lookup on a map.
-- Returns HiValueNull if element was not found.
-- One might wonder why this function accepts the value wrapped in an array.
-- This was done to avoid excessive pattern-matching in evalWithArgs, to ensure
-- that HiErrorInvalidFunction will be thrown when needed.
hiFunDictLookUp
  :: HiMonad m
  => Map HiValue HiValue       -- ^ Map to lookup in
  -> [HiValue]                 -- ^ Value to be looked up
  -> ExceptT HiError m HiValue -- ^ The result of lookup
hiFunDictLookUp m [a] = case M.lookup a m of
  (Just v) -> return v
  Nothing  -> return HiValueNull
hiFunDictLookUp _ _ = throwE HiErrorArityMismatch

-- | Helper function to perform indexing/slicing on listable types.
hiFunListOperation
  :: (Sliceable a, Indexable a, HiMonad m) -- ^ Helper type classes to avoid code duplication
  => a                                     -- ^ Listable type to perform operation on
  -> [HiValue]                             -- ^ Indexes to perform list actions
  -> ExceptT HiError m HiValue             -- ^ List operation result
hiFunListOperation s [HiValueNumber (posI :% 1)] = return $ s `atOrNull` fromIntegral posI
hiFunListOperation s [HiValueNumber (posI :% 1), HiValueNumber (posJ :% 1)] = return $ slice (fromIntegral posI) (fromIntegral posJ) s
hiFunListOperation s [HiValueNull, HiValueNumber (posJ :% 1)] = return $ slice 0 (fromIntegral posJ) s
hiFunListOperation s [HiValueNumber (posI :% 1), HiValueNull] = return $ slice (fromIntegral posI) (len s) s
hiFunListOperation s [HiValueNull, HiValueNull] = return $ slice 0 (len s) s
hiFunListOperation _ _ = throwE HiErrorInvalidArgument

-- | Helper function to perform stimes on semigroup types to avoid duplicate code
hiFunListStimes :: (HiMonad m, Semigroup a) => Integer -> a -> ExceptT HiError m a
hiFunListStimes n s = if n < 1
                      then throwE HiErrorInvalidArgument
                      else return $ stimes n s

-- | Get arity of a function.
-- Specify -1 if the function can accept any number of arguments
getArity :: HiFun -> Int
getArity HiFunAdd            = 2
getArity HiFunSub            = 2
getArity HiFunMul            = 2
getArity HiFunDiv            = 2
getArity HiFunNot            = 1
getArity HiFunAnd            = 2
getArity HiFunOr             = 2
getArity HiFunLessThan       = 2
getArity HiFunGreaterThan    = 2
getArity HiFunEquals         = 2
getArity HiFunNotLessThan    = 2
getArity HiFunNotGreaterThan = 2
getArity HiFunNotEquals      = 2
getArity HiFunIf             = 3
getArity HiFunLength         = 1
getArity HiFunToUpper        = 1
getArity HiFunToLower        = 1
getArity HiFunReverse        = 1
getArity HiFunTrim           = 1
getArity HiFunList           = -1
getArity HiFunRange          = 2
getArity HiFunFold           = 2
getArity HiFunPackBytes      = 1
getArity HiFunUnpackBytes    = 1
getArity HiFunEncodeUtf8     = 1
getArity HiFunDecodeUtf8     = 1
getArity HiFunZip            = 1
getArity HiFunUnzip          = 1
getArity HiFunSerialise      = 1
getArity HiFunDeserialise    = 1
getArity HiFunRead           = 1
getArity HiFunWrite          = 2
getArity HiFunMkDir          = 1
getArity HiFunChDir          = 1
getArity HiFunParseTime      = 1
getArity HiFunRand           = 2
getArity HiFunEcho           = 1
getArity HiFunKeys           = 1
getArity HiFunValues         = 1
getArity HiFunInvert         = 1
getArity HiFunCount          = 1

-- | Mark that short circuit evaluation is possible for a function
allowShortCircuit :: HiValue -> Bool
allowShortCircuit (HiValueFunction HiFunIf)  = True
allowShortCircuit (HiValueFunction HiFunAnd) = True
allowShortCircuit (HiValueFunction HiFunOr)  = True
allowShortCircuit _                          = False
