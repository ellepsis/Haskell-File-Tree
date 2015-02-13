module Main where

import System.Directory 
import System.Environment (getArgs)
import System.Mem
import System.IO
import Text.Printf
import Control.Applicative
import Control.Monad
import Control.Exception as E
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.String (unlines)

type FileName = String

data FileTree = 
	File {name :: Text} 
	| Dir {name :: Text, contents :: [FileTree] } 
	| Failed {name :: Text} deriving (Eq)

instance Show FileTree where
	show tree = unlines $ showWithLevel 0 tree 
		where
			showWithLevel :: Int -> FileTree -> [String]
			showWithLevel level (Dir name contents) = 
				(replicate level '-' ++ unpack(name)) : (join $ map (showWithLevel (level+1)) contents)
			showWithLevel level (File name) = [replicate level '-' ++ unpack(name)]
			showWithLevel level (Failed name) = [replicate level '-' ++ unpack(name)]

-- Bugs file size with empty start value

main = do
	hSetEncoding stdout utf8
	args <- getArgs
	path <- (\x -> if (length x > 0) then return (head x) else getLine) args
	list <- Dir (pack $ init path) <$> getSubDirContents path
	putStrLn "\n"
	putStrLn $ show list
	directoryAndFilesCount <- return $ countFilesAndDirectories list
	putStrLn ("Directories "++ (show . sel2 $  directoryAndFilesCount))
	putStrLn ("Files " ++ (show . sel1 $  directoryAndFilesCount))
	putStrLn ("Permission errors " ++ (show . sel3 $  directoryAndFilesCount))
	input <- getLine
	dirSize <- directorySize "" list
	putStrLn ("Size " ++ (show . sel1 $ dirSize))
	putStrLn (show (length . sel2 $ dirSize) ++" Errors :" ++ (unlines . sel2 $ dirSize))

-- Current directory -> File path
getSubDirContents :: FilePath -> IO [FileTree]
getSubDirContents dirPath = do
	contents <- filter (\x -> x /= "." && x /= ".." ) <$> tryReadContent dirPath
	if (contents == ["\NUL"]) then return $ errorPathFileTree dirPath
		else do
			isDirs <-  sequence $ map doesDirectoryExist $! map (dirPath++) $ map ('/':) contents
			createTree dirPath contents isDirs				
	
-- Current directory -> Directory Contents -> Is Directory
createTree :: FilePath -> [FilePath] -> [Bool] -> IO [FileTree]
createTree currentDir filePaths isDir = do
	a <- sequence( zipWith (\x y -> 
		if (x==True) 
			then ( do 
				subDirContents <- getSubDirContents (currentDir++('/' : y))
				performMinorGC
				return $! Dir (pack y) subDirContents )
			else return (File (pack y))) isDir filePaths)
	return a

fileNameFromPath :: FilePath -> FileName
fileNameFromPath fp = last (splitOn "/" fp)

errorPathFileTree :: FilePath -> [FileTree]
errorPathFileTree path = [Failed $ pack path]

-- Return '\NUL' if were read error
tryReadContent :: FilePath -> IO [FilePath]
tryReadContent path = E.handle possibleError $! getDirectoryContents path
	where 
		possibleError :: IOException -> IO [FilePath]
		possibleError _ = return [['\NUL']]

checkFail :: [FilePath] -> Bool
checkFail [] = False
checkFail list = last (head list) == '\NUL'

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

-- Return all posible non error paths to files from a tree
allFilePath :: FileTree -> [FilePath]
allFilePath (Dir name contents) = map (((unpack name)++"/")++) (join (map allFilePath contents))
allFilePath (File name) = [unpack name] 
allFilePath (Failed _) = []