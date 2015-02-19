module TreeOperations where

import FileTree
import System.IO
import Control.Applicative
import Control.Monad
import Control.Exception as E
import Data.List (foldl', (\\), intersect, union)
import Data.List.Split (splitOn)
import qualified Data.Map as M 
import Data.Text (Text, pack, unpack)

data PathDifference = PathDifference FilePath [Difference]  deriving (Eq)
data Difference = Added FileTree | Removed FileTree  deriving (Eq)

instance Show PathDifference where
    show (PathDifference path list) = "Folder:" ++ path ++ "\n" ++ (unlines $ map show list) 

instance Show Difference where
    show (Added tree)   = "--> " ++ (showTreeName tree)
    show (Removed tree) = "<-- " ++ (showTreeName tree)

-- FileTree -> (File count, Directory count, Error count)
countFilesAndDirectories :: FileTree -> (Int, Int, Int)
countFilesAndDirectories (File _) = (1, 0, 0)
countFilesAndDirectories (Dir _ contents) = foldl' (\(a,b,c) (a1,b1,c1) -> (a+a1,b+b1,c+c1)) (0,1,0) $ map countFilesAndDirectories contents
countFilesAndDirectories (Failed _) = (0,0,1)

-- Return file size in bytes
getFileSize :: FilePath -> IO (Either Integer String)
getFileSize path = E.handle possibleError $ 
    bracket (openFile path ReadMode) hClose $ \h -> do 
        size <- hFileSize h
        return (Left size)
        where 
            possibleError :: IOException -> IO (Either Integer String)
            possibleError _ = return $ Right path

-- Path to directory -> Tree -> (Bytes count, Path where was an error)
-- Path to directory will be concatenated with name of Tree top level directory 
directorySize :: FilePath -> FileTree -> IO (Integer, [String])
directorySize filePath fileTree =
    if (filePath == (unpack . name) fileTree) 
        then foldl' sumSizes (0, []) <$> (sequence (map getFileSize $ allFilePath fileTree)) 
        else foldl' sumSizes (0, []) <$> (sequence (map getFileSize $ map (filePath++) $ allFilePath fileTree)) 
            where  
                sumSizes :: (Integer, [String]) -> Either Integer String -> (Integer, [String])
                sumSizes (allSize, errors) (Left size) = (allSize+size, errors)
                sumSizes (allSize, errors) (Right string) = (allSize, string : errors)

allFileNames :: FileTree -> [FileName]
allFileNames (Dir name contents) = join (map allFilePath contents)
allFileNames (File name) = [unpack name] 
allFileNames (Failed _) = []

extentionCount :: FileTree -> M.Map String Int 
extentionCount tree = foldl' (\map elem -> (M.insertWith (+) (key elem) 1 map)) M.empty (allFileNames tree)
    where
        key :: FileName -> String
        key name = if elem '.' name 
            then last $ splitOn "." name
            else "."

--Разобраться с отображением путей
treeDifference :: FileTree -> FileTree -> FilePath -> [PathDifference]
treeDifference dir1@(Dir name1 contents1) dir2@(Dir name2 contents2) path = 
    let diff = union (contents1 \\ contents2) (contents2 \\ contents1)
        diffDirs = filter isDir diff
        --Differense files and directories only in current directory
        diffInThisFolder = diff \\ (join $ map (\(x,y) -> [x,y]) $ getTreesWithTheSameName diffDirs)
        thisDirectoryPathDifference = PathDifference path $ map
            (\x -> if (elem x contents1) then Removed x else Added x)
            diffInThisFolder
        subDirectoryDifferences = join $ map 
            (\(x,y) -> treeDifference x y $ addSubDirToPath path (unpack $ name x))
            $ getTreesWithTheSameName diffDirs
    in filter isNoEmptyDiff $ thisDirectoryPathDifference:subDirectoryDifferences
    where
        isNoEmptyDiff :: PathDifference -> Bool
        isNoEmptyDiff (PathDifference path []) = False
        isNoEmptyDiff _ = True
treeDifference file1@(File name1) file2@(File name2) path = [PathDifference path [Removed file1, Added file2]]

getTreesWithTheSameName :: [FileTree] -> [(FileTree, FileTree)]
getTreesWithTheSameName [] = []
getTreesWithTheSameName (x:xs) = let
    elems = filter (\y -> (name y) == (name x)) xs
    in if (length(elems) >0 ) 
        then (x,(head elems)):(getTreesWithTheSameName xs)   
        else getTreesWithTheSameName xs

printSizeInHumanReadableFormat :: Integer -> String
printSizeInHumanReadableFormat bytes 
    | quot bytes (2^40) > 0 = (show $ quot bytes (2^40)) ++ " TiB " ++ (printSizeInHumanReadableFormat $ mod bytes (2^40))
    | quot bytes (2^30) > 0 = (show $ quot bytes (2^30)) ++ " GiB " ++ (printSizeInHumanReadableFormat $ mod bytes (2^30))
    | quot bytes (2^20) > 0 = (show $ quot bytes (2^20)) ++ " MiB " ++ (printSizeInHumanReadableFormat $ mod bytes (2^20))
    | quot bytes (2^10) > 0 = (show $ quot bytes (2^10)) ++ " KiB " ++ (printSizeInHumanReadableFormat $ mod bytes (2^10))
    | otherwise = (show bytes) ++ " Bytes " 

addSubDirToPath :: String -> String-> String
addSubDirToPath [] path = path
addSubDirToPath path subDir = if (last path == '/') then path++subDir else path ++ "/" ++ subDir