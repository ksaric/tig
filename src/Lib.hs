{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( testGit
    , gitCommitStatus
    -- * Git commands
    , gitInit
    , gitClear
    , gitVersion
    , gitStatus
    , gitAdd
    , gitCommit
    -- * Status and parse
    , gitStatusWithParse
    -- * Test values
    -- * Initial
    , testEmpty
    , testUntracked
    , testTrackedFile
    -- * Non initial
    , testCommitedFile
    , testTrackedNonInitial
    , testUntrackedNonInitial
    , testModifiedFile
    , testModifiedFileAfterCommit
    -- * Types
    , module Types
    ) where

import           Universum

import           Text.Parsec      (Parsec, anyChar, char, manyTill, newline,
                                   parse, spaces, string)
import qualified Text.Parsec      as P

import           System.Directory (createDirectory, doesDirectoryExist,
                                   removeDirectoryRecursive,
                                   withCurrentDirectory)
import           System.Process   (readProcess)

import           Types

import           Test.QuickCheck  (arbitrary, generate)

------------------------------------------------------------
-- Git commands
------------------------------------------------------------

gitInit :: IO Text
gitInit =
    fromString <$> readProcess "git" ["init"]                   []

gitClear :: IO Text
gitClear =
    fromString <$> readProcess "rm"  ["-rf", ".git"]            []

gitVersion :: IO Text
gitVersion =
    fromString <$> readProcess "git" ["version"]                []

gitStatus :: IO Text
gitStatus =
    fromString <$> readProcess "git" ["status"]                 []

gitAdd :: String -> IO Text
gitAdd fileName =
    fromString <$> readProcess "git" ["add", fileName]          []

gitCommit :: String -> IO Text
gitCommit message =
    fromString <$> readProcess "git" ["commit", "-m", message]  []

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

gitBranchFromStatus :: Parsec Text () BranchName
gitBranchFromStatus = do
    _           <- string "On branch "
    branchName  <- manyTill anyChar (P.try $ char '\n')
    pure . BranchName $ branchName

-- parseTest gitInitialCommitStatus testEmpty
gitInitialCommitStatus :: Parsec Text () GitStatusHeader
gitInitialCommitStatus = do
    branchName  <- gitBranchFromStatus
    _           <- newline
    _           <- string "Initial commit\n"
    _           <- newline
    pure $ InitialCommit branchName


-- parseTest gitNonInitialCommitStatus testCommitedFile
gitNonInitialCommitStatus :: Parsec Text () GitStatusHeader
gitNonInitialCommitStatus = do
    branchName      <- gitBranchFromStatus
    pure $ NonInitialCommit branchName

gitHeaderParse :: Parsec Text () GitStatusHeader
gitHeaderParse =
    P.try   gitInitialCommitStatus
    <|>     gitNonInitialCommitStatus

gitCleanFiles :: Parsec Text () String
gitCleanFiles = string "nothing to commit" *> manyTill anyChar (P.try $ char '\n')

-- | Get a list of untracked files.
gitUntrackedFiles :: Parsec Text () [String]
gitUntrackedFiles = do
    _               <- string "Untracked files:\n"
    _               <- string "  (use \"git add <file>...\" to include in what will be committed)\n"
    _               <- newline
    files           <- char '\t' *> manyTill anyChar (P.try $ char '\n')
    _               <- newline
    _               <- string "nothing added to commit but untracked files present (use \"git add\" to track)\n"
    pure [files]

-- | Get a list of tracked files.
gitTrackedFiles :: Parsec Text () [String]
gitTrackedFiles = do
    _               <- string "Changes to be committed:\n"
    _               <- string "  (use \"" *> manyTill anyChar (P.try $ char '\n')
    _               <- newline
    files           <- string "\tnew file:" *> spaces *> manyTill anyChar (P.try $ char '\n')
    pure [files]


gitUnstagedFiles :: Parsec Text () [String]
gitUnstagedFiles = do
    _               <- string "Changes not staged for commit:\n"
    _               <- string "  (use \"git add <file>...\" to update what will be committed)\n"
    _               <- string "  (use \"git checkout -- <file>...\" to discard changes in working directory)\n"
    _               <- newline
    files           <- string "\tmodified:" *> spaces *> manyTill anyChar (P.try $ char '\n')
    _               <- newline
    _               <- string "no changes added to commit (use \"git add\" and/or \"git commit -a\")\n"
    pure [files]


gitStagedFiles :: Parsec Text () [String]
gitStagedFiles = do
    _               <- string "Changes to be committed:\n"
    _               <- string "  (use \"git reset HEAD <file>...\" to unstage)\n"
    _               <- newline
    files           <- string "\tmodified:" *> spaces *> manyTill anyChar (P.try $ char '\n')
    pure [files]

-- | The part of the parser that deals with the alternatives
gitCommitStatus :: Parsec Text () GitStatus
gitCommitStatus =
        tryCleanFiles
    <|> tryUntrackedFiles
    <|> tryTrackedFiles
    <|> tryUnstagedFiles
    <|> tryStagedFiles
  where
    tryCleanFiles :: Parsec Text () GitStatus
    tryCleanFiles = P.try $ do
        gitHeader       <- gitHeaderParse
        _               <- gitCleanFiles
        pure Clean
            { gsHeader          = gitHeader
            }

    tryUntrackedFiles :: Parsec Text () GitStatus
    tryUntrackedFiles = P.try $ do
        gitHeader       <- gitHeaderParse
        untrackedFiles  <- gitUntrackedFiles
        pure Untracked
            { gsHeader          = gitHeader
            , gsUntrackedFiles  = untrackedFiles
            }

    tryTrackedFiles :: Parsec Text () GitStatus
    tryTrackedFiles = P.try $ do
        gitHeader       <- gitHeaderParse
        trackedFiles    <- gitTrackedFiles
        pure Tracked
            { gsHeader          = gitHeader
            , gsTrackedFiles    = trackedFiles
            }

    tryUnstagedFiles :: Parsec Text () GitStatus
    tryUnstagedFiles = P.try $ do
        gitHeader       <- gitHeaderParse
        trackedFiles    <- gitUnstagedFiles
        pure Unstaged
            { gsHeader          = gitHeader
            , gsUnstagedFiles   = trackedFiles
            }

    tryStagedFiles :: Parsec Text () GitStatus
    tryStagedFiles = P.try $ do
        gitHeader       <- gitHeaderParse
        trackedFiles    <- gitStagedFiles
        pure Staged
            { gsHeader          = gitHeader
            , gsStagedFiles     = trackedFiles
            }

-- | Get status and parse it, returning @GitStatus@.
gitStatusWithParse :: IO (Either P.ParseError GitStatus)
gitStatusWithParse = do
    resultGitStatus <- gitStatus
    pure $ parse gitCommitStatus "status" resultGitStatus

------------------------------------------------------------
-- Examples
------------------------------------------------------------

testEmpty :: Text
testEmpty = unlines
    [ "On branch master"
    , ""
    , "Initial commit"
    , ""
    , "nothing to commit (create/copy files and use \"git add\" to track)"
    ]

testUntracked :: Text
testUntracked = unlines
    [ "On branch master"
    , ""
    , "Initial commit"
    , ""
    , "Untracked files:"
    , "  (use \"git add <file>...\" to include in what will be committed)"
    , ""
    , "\ttest_file.txt"
    , ""
    , "nothing added to commit but untracked files present (use \"git add\" to track)"
    , ""
    ]

testTrackedFile :: Text
testTrackedFile = unlines
    [ "On branch master"
    , ""
    , "Initial commit"
    , ""
    , "Changes to be committed:"
    , "  (use \"git rm --cached <file>...\" to unstage)"
    , ""
    , "\tnew file:   test_file.txt"
    , ""
    ]

------------------------------------------------------------

testCommitedFile :: Text
testCommitedFile = unlines
    [ "On branch master"
    , "nothing to commit, working directory clean"
    ]

testUntrackedNonInitial :: Text
testUntrackedNonInitial = unlines
    [ "On branch master"
    , "Untracked files:"
    , "  (use \"git add <file>...\" to include in what will be committed)"
    , ""
    , "\tfile1.txt"
    , ""
    , "nothing added to commit but untracked files present (use \"git add\" to track)"
    , ""
    ]

testTrackedNonInitial :: Text
testTrackedNonInitial = unlines
    [ "On branch master"
    , "Changes to be committed:"
    , "  (use \"git reset HEAD <file>...\" to unstage)"
    , ""
    , "\tnew file:   file1.txt"
    , ""
    , ""
    ]

testModifiedFile :: Text
testModifiedFile = unlines
    [ "On branch master"
    , "Changes not staged for commit:"
    , "  (use \"git add <file>...\" to update what will be committed)"
    , "  (use \"git checkout -- <file>...\" to discard changes in working directory)"
    , ""
    , "\tmodified:   test_file.txt"
    , ""
    , "no changes added to commit (use \"git add\" and/or \"git commit -a\")"
    ]

testModifiedFileAfterCommit :: Text
testModifiedFileAfterCommit = unlines
    [ "On branch master"
    , "Changes to be committed:"
    , "  (use \"git reset HEAD <file>...\" to unstage)"
    , ""
    , "\tmodified:   test_file.txt"
    ]

------------------------------------------------------------

-- | An example of how the Git statuses can be parsed.
testGit :: HasCallStack => IO ()
testGit = do
    -- create test directory
    let testDirectoryName = "gittest"

    -- If exists, remove it.
    whenM (doesDirectoryExist testDirectoryName) $ do
        removeDirectoryRecursive testDirectoryName

    _ <- createDirectory testDirectoryName

    fileName    <- getFileName      <$> generate arbitrary
    fileContent <- getFileContent   <$> generate arbitrary

    -- check Git version
    resultGitVersion  <- gitVersion

    putTextLn "------------------------"
    putTextLn resultGitVersion
    putTextLn "------------------------"

    _ <- withCurrentDirectory testDirectoryName $ do
        _ <- writeFile fileName fileContent


        resultGitInit           <- gitInit
        resultGitStatus         <- gitStatus
        resultGitAdd            <- gitAdd fileName
        resultGitAddStatus      <- gitStatus
        resultGitCommit         <- gitCommit "First commit."
        resultGitCommitStatus   <- gitStatus

        putTextLn "------------------------"
        putTextLn resultGitInit
        putTextLn "------------------------"

        putTextLn "------------------------"
        putTextLn . show $ parse gitCommitStatus "status" resultGitStatus
        putTextLn "------------------------"

        putTextLn "------------------------"
        putTextLn resultGitAdd
        putTextLn "------------------------"

        putTextLn "------------------------"
        putTextLn . show $ parse gitCommitStatus "status" resultGitAddStatus
        putTextLn "------------------------"

        putTextLn "------------------------"
        putTextLn resultGitCommit
        putTextLn "------------------------"

        putTextLn "------------------------"
        putTextLn . show $ parse gitCommitStatus "status" resultGitCommitStatus
        putTextLn "------------------------"

    pure ()

