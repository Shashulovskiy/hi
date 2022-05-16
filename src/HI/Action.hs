module HI.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  , runHiAction
  ) where

import Control.Exception.Base (Exception, throw)
import Control.Monad (ap, liftM)
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

import HI.Base

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq, Enum, Bounded)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a
  (<*>) = ap

instance Monad HIO where
  a >>= f = HIO $ \p -> do
    tmp <- runHIO a p
    runHIO (f tmp) p

instance HiMonad HIO where
  runAction a = HIO $ \p -> do
    -- Find out if all required permissions were satisfied
    let permissionDifference = requiredPermissions a `Set.difference` p
     in if Set.null permissionDifference
          then runHiAction a
          -- Some permissions are missing, throw an error
          else throw $ PermissionRequired $ Set.elemAt 0 permissionDifference

-- | Runs HiAction inside IO
runHiAction
  :: HiAction   -- ^ HiAction to run
  -> IO HiValue -- ^ The result of running provided action
runHiAction (HiActionRead path) = do
  -- Check if provided path is a directory or file (or nothing)
  isDirectory <- doesDirectoryExist path
  if isDirectory
    -- If directory, pack and return a list of its content
    then HiValueList . S.fromList . fmap (HiValueString . T.pack) <$> listDirectory path
    else do
      -- If file, read and return its contents
      fileContents <- B.readFile path
      case decodeUtf8' fileContents of
        (Right text) -> return $ HiValueString text
        -- Unable to decode file, return raw bytes
        (Left _)     -> return $ HiValueBytes fileContents
runHiAction (HiActionWrite f c) = HiValueNull <$ B.writeFile f c
runHiAction (HiActionMkDir dir) = HiValueNull <$ createDirectory dir
runHiAction (HiActionChDir dir) = HiValueNull <$ setCurrentDirectory dir
runHiAction HiActionCwd = HiValueString . T.pack <$> getCurrentDirectory
runHiAction HiActionNow = HiValueTime <$> getCurrentTime
runHiAction (HiActionRand l r) = if (l < (maxBound :: Int) && l > (minBound :: Int)) || (r < (maxBound :: Int) && r > (minBound :: Int))
                                   then HiValueNumber . toRational <$> getStdRandom (uniformR (l, r))
                                   else return HiValueNull
runHiAction (HiActionEcho s) = HiValueNull <$ print s

-- | Get required permissions for an action
requiredPermissions
  :: HiAction             -- ^ Action to get permissions for
  -> Set HiPermission     -- ^ Set of permissions required for given action
requiredPermissions (HiActionRead _)    = Set.fromList [AllowRead]
requiredPermissions (HiActionWrite _ _) = Set.fromList [AllowWrite]
requiredPermissions (HiActionMkDir _)   = Set.fromList [AllowWrite]
requiredPermissions (HiActionChDir _)   = Set.fromList [AllowRead]
requiredPermissions HiActionCwd         = Set.fromList [AllowRead]
requiredPermissions HiActionNow         = Set.fromList [AllowTime]
requiredPermissions (HiActionRand _ _)  = Set.empty
requiredPermissions (HiActionEcho _)    = Set.fromList [AllowWrite]
