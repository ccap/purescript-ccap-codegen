module Ccap.Codegen.Util where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.String.Regex (regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global, multiline) as Regex.Flags
import Data.String.Utils as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Node.Process as Process

liftEffectSafely :: forall a. Effect a -> ExceptT String Aff a
liftEffectSafely = ExceptT <<< liftEffect <<< map (lmap Error.message) <<< try

processResult :: ExceptT String Aff Unit -> Aff Unit
processResult r = do
  e <- runExceptT r
  e # either
    (\s -> liftEffect $ do
      Console.error $ "ERROR: " <> s
      Process.exit 1)
    pure

--| Clean up output from purescript-boxes
scrubEolSpaces :: String -> String
scrubEolSpaces i =
  regex " +$" (Regex.Flags.multiline <> Regex.Flags.global) # either
    (const i)
    (\r -> Regex.replace r "" i)

--| Ensure newline
ensureNewline :: String -> String
ensureNewline s = if String.endsWith "\n" s then s else s <> "\n"
