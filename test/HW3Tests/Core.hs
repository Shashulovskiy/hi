module HITests.Core
  ( module HI.Base
  , module HITests.Core
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import HI.Base
import HI.Evaluator (eval)
import HI.Parser (parse)
import HI.Pretty (prettyValue)
import Test.Hspec

data Result =
    Ok String
  | EvalError HiError
  | ParseError
  deriving (Show, Eq)

instance HiMonad Identity where
  runAction = error "Not supposed to be called"

evalTest :: String -> Result
evalTest test = case parse test of
  (Right expression) -> case runIdentity $ eval expression of
    (Right value)    -> Ok $ show $ prettyValue value
    (Left evalError) -> EvalError evalError
  (Left _) -> ParseError

infix 1 =?
(=?) :: HasCallStack => String -> Result -> Expectation
actual =? expected = evalTest actual `shouldBe` expected
