module Main
    ( main
    ) where

import           Universum

import           System.Directory        (createDirectory, doesDirectoryExist,
                                          removeDirectoryRecursive,
                                          withCurrentDirectory)

import           Text.Parsec             (ParseError)

import           Test.Hspec              (Spec, describe, hspec, it, parallel)
import           Test.Hspec.QuickCheck   (modifyMaxSuccess)
import           Test.QuickCheck         (arbitrary, forAll)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Lib


main :: IO ()
main = hspec spec

-- stack test tig
spec :: Spec
spec =
    describe "tig" $ do
        describe "Simple scenarios" $ do
            versionShouldBeCorrect
            initArbitraryDirectoryShouldBeOk


versionShouldBeCorrect :: Spec
versionShouldBeCorrect = parallel $ do
    describe "current version" $ modifyMaxSuccess (const 1000) $ do
        it "should be 2.7.4" $ do
            monadicIO $ do
                -- check Git version
                resultGitVersion  <- run gitVersion
                assert $ resultGitVersion == "git version 2.7.4\n"

initArbitraryDirectoryShouldBeOk :: Spec
initArbitraryDirectoryShouldBeOk =
    describe "The simple empty directory" $ modifyMaxSuccess (const 1000) $ do

        it "should be always initialized for a single subdirectory" $ do
            forAll arbitrary $ \(directoryName :: DirectoryName) -> do
                monadicIO $ do

                    resultGitStatus <- run . createDirectoryInitStatus $ [directoryName]

                    -- always must be successful init
                    assert $ isRight resultGitStatus

                    -- always must be clean status
                    whenRight resultGitStatus $ \resGitStatus ->
                        assert $ isClean resGitStatus

        it "should be always initialized for multiple subdirectories" $ do
            forAll arbitrary $ \(directoryNames :: DirectoryNames) -> do
                monadicIO $ do

                    resultGitStatus <- run . createDirectoryInitStatus $ getDirectoryNames directoryNames

                    -- always must be successful init
                    assert $ isRight resultGitStatus

                    -- always must be clean status
                    whenRight resultGitStatus $ \resGitStatus ->
                        assert $ isClean resGitStatus

  where
    createDirectoryInitStatus :: [DirectoryName] -> IO (Either ParseError GitStatus)
    createDirectoryInitStatus directoryNames = do
        let testDirectoryName = "gittest"

        -- If exists, remove it.
        whenM (doesDirectoryExist testDirectoryName) $ do
            removeDirectoryRecursive testDirectoryName

        _ <- createDirectory testDirectoryName

        -- inside test directory
        withCurrentDirectory testDirectoryName $ do

            _ <- createDirectoryFromDirectories directoryNames

            let directoryName = getDirectoryFromDirectories directoryNames

            --_ <- createDirectory directoryName
            withCurrentDirectory directoryName $ do

                -- first initialize the repository
                _               <- gitInit

                -- then let's return the parsed status
                gitStatusWithParse


