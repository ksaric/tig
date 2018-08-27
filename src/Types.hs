module Types
    ( GitStatus (..)
    , GitStatusHeader (..)
    , BranchName (..)
    , UntrackedFile (..)
    -- fs
    , DirectoryName (..)
    , DirectoryNames (..)
    , getDirectoryFromDirectories
    , createDirectoryFromDirectories
    , FileContent (..)
    , FileName (..)
    -- * util
    , isClean
    ) where

import           Universum

import           System.Directory (createDirectory, withCurrentDirectory)
import           System.FilePath  (pathSeparator)

import           Test.QuickCheck  (Arbitrary (..), elements, listOf1, suchThat,
                                   vectorOf)


------------------------------------------------------------

newtype BranchName = BranchName { getBranchName :: String }
    deriving (Show, Eq)

data GitStatusHeader
    = InitialCommit     { gshBranchName :: BranchName }
    | NonInitialCommit  { gshBranchName :: BranchName }
    deriving (Show, Eq)

data GitStatus
    = Clean     { gsHeader :: GitStatusHeader }
    -- Changes to new files
    | Tracked   { gsHeader :: GitStatusHeader, gsTrackedFiles :: [String] }
    | Untracked { gsHeader :: GitStatusHeader, gsUntrackedFiles :: [String] }
    -- Changes to existing files
    | Unstaged  { gsHeader :: GitStatusHeader, gsUnstagedFiles :: [String] }
    | Staged    { gsHeader :: GitStatusHeader, gsStagedFiles :: [String] }
    deriving (Show, Eq)

isClean :: GitStatus -> Bool
isClean (Clean _) = True
isClean _         = False

------------------------------------------------------------

newtype UntrackedFile = UntrackedFile { getUntrackedFile :: String }
    deriving (Show, Eq)

------------------------------------------------------------

newtype DirectoryNames = DirectoryNames { getDirectoryNames :: [DirectoryName] }
    deriving (Show, Eq)

instance Arbitrary DirectoryNames where
    arbitrary = do
        dirNames <- listOf1 arbitrary `suchThat` limitDirectoryNamesLength

        pure . DirectoryNames $ dirNames
      where
        limitDirectoryNamesLength :: [DirectoryName] -> Bool
        limitDirectoryNamesLength directoryNames =
            (length $ getDirectoryFromDirectories directoryNames) < 100


createDirectoryFromDirectories :: [DirectoryName] -> IO ()
createDirectoryFromDirectories []       = pure ()
createDirectoryFromDirectories (x:xs)   = do
    let dirName = getDirectoryName x

    _ <- createDirectory dirName
    withCurrentDirectory dirName $
        createDirectoryFromDirectories xs


getDirectoryFromDirectories :: [DirectoryName] -> String
getDirectoryFromDirectories directoryNames =
   let dirNames :: [FilePath]
       dirNames = map getDirectoryName directoryNames

       directoryName = intercalate [pathSeparator] dirNames
   in  directoryName

------------------------------------------------------------

newtype DirectoryName = DirectoryName { getDirectoryName :: String }
    deriving (Show, Eq)

instance Arbitrary DirectoryName where
    arbitrary = do
        let validChars = ['A'..'Z'] <> ['0'..'9'] <> ['a'..'z'] <> ['-', '_', ' ']
        dirNameSize     <- arbitrary `suchThat` (>= 1)
        dirName         <- vectorOf dirNameSize (elements validChars)
        pure . DirectoryName $ dirName

------------------------------------------------------------

newtype FileContent = FileContent { getFileContent :: Text }
    deriving (Show, Eq)

instance Arbitrary FileContent where
    arbitrary = FileContent . fromString <$> arbitrary

------------------------------------------------------------

-- putTextLn . fromString =<< generate ((sublistOf <=< shuffle) validChars :: Gen [Char])
newtype FileName = FileName { getFileName :: String }

-- getFileName <$> generate arbitrary
instance Arbitrary FileName where
    arbitrary = do
        let validChars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['-', '_', ' ']
        --fileName        <- fromString <$> (sublistOf <=< shuffle) validChars
        fileNameSize    <- arbitrary `suchThat` (>= 1)
        fileName        <- vectorOf fileNameSize (elements validChars)
        fileExtension   <- elements ["txt", "bin", "hs"] -- TODO(ks): More random!
        pure . FileName $ fileName <> "." <> fileExtension

------------------------------------------------------------
