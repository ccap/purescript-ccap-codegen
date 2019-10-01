module Test.Ccap.Codegen.FileSystem
  ( specs
  ) where

import Prelude

import Ccap.Codegen.FileSystem (mkDirP)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.FS.Sync as Sync
import Node.Path as Path
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

specs :: Spec Unit
specs =
  let
    target = "./test/resources/"
    fstLevel = Path.concat [ target, "mkDirP" ]
    sndLevel = Path.concat [ fstLevel, "second-level" ]

    makeDir = (_ `shouldEqual` (Right unit)) <=< liftAff <<< runExceptT <<< mkDirP
    exists = (_ `shouldEqual` true) <=< liftEffect <<< Sync.exists
    doesNotExist = (_ `shouldEqual` false) <=< liftEffect <<< Sync.exists
    rmdir = liftEffect <<< Sync.rmdir

    checkExists dir = it ("Check that " <> dir <> " exists") $ exists dir
    checkDoesNotExist dir = it ("Check that " <> dir <> " does not exist") $ doesNotExist dir
    checkMakeDir dir = it ("Make " <> dir <> " doesn't fail") $ makeDir dir
    checkMade dir = it (dir <> " was created") $ exists dir *> rmdir dir *> doesNotExist dir
  in describe "mkdir -p" do
    describe (target <> " does nothing") do
      checkExists target
      checkMakeDir target
    describe (fstLevel <> " creates the directory") do
      checkExists target
      checkDoesNotExist fstLevel
      checkMakeDir fstLevel
      checkMade fstLevel
    describe (sndLevel <> " creates both directories") do
      checkExists target
      checkDoesNotExist fstLevel
      checkDoesNotExist sndLevel
      checkMakeDir sndLevel
      checkMade sndLevel
      checkMade fstLevel
