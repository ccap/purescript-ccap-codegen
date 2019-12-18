module Test.Ccap.Codegen.FileSystem
  ( specs
  ) where

import Prelude
import Ccap.Codegen.FileSystem (isFile, mkDirP, readTextFile)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.FS.Sync as Sync
import Node.Path as Path
import Test.Ccap.Codegen.Util (shouldBeLeft)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

specs :: Spec Unit
specs =
  let
    target = "./test/resources/filesystem"

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
  in
    describe "file system helpers" do
      describe "mkdir -p" do
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
      describe "is file" do
        it "Returns false if the path doesn't exist" do
          result <- liftEffect $ isFile $ Path.concat [ target, "not a real path" ]
          result `shouldEqual` false
        it "Returns false if the path exists but is a folder" do
          result <- liftEffect $ isFile target
          result `shouldEqual` false
        it "Returns true if the path exists and is a file" do
          result <- liftEffect $ isFile $ Path.concat [ target, "isFile" ]
          result `shouldEqual` true
      describe "read text file" do
        it "Returns an error if the path doesn't exist" do
          result <- liftEffect $ readTextFile $ Path.concat [ target, "not a real path" ]
          shouldBeLeft result
        it "Returns an error if the path exists but is a folder" do
          result <- liftEffect $ readTextFile target
          shouldBeLeft result
        it "Returns a string of a files contents" do
          result <- liftEffect $ readTextFile $ Path.concat [ target, "readTextFile" ]
          result `shouldEqual` (Right "hello\nworld\n")
