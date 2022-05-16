{-# LANGUAGE BlockArguments #-}

module Main where

  import System.Console.Haskeline (runInputT, InputT, defaultSettings, outputStrLn, getInputLine)
  import HI.Parser
  import HI.Evaluator
  import HI.Pretty
  import HI.Action
  import qualified Data.Set as S
  import Control.Monad.Reader (liftIO)

  main :: IO ()
  main = runInputT defaultSettings loop
    where
      loop :: InputT IO ()
      loop = do
        minput <- getInputLine "hi> "
        case minput of
          Nothing -> return ()
          Just ":q" -> return ()
          Just input -> do let parsedInput = parse input
                           case parsedInput of
                             (Right v) -> do
                               value <- liftIO $ (runHIO $ eval v) (S.fromList [AllowRead, AllowWrite, AllowTime])
                               outputStrLn $ show $ prettyResult value
                             (Left err) -> do
                               outputStrLn $ show $ prettyParseError err
                           loop