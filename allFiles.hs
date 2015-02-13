module Main where

import System.Directory 
import System.Environment (getArgs)
import System.Mem
import Control.Applicative
import Control.Monad
import Control.Exception (IOException, handle)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Text (Text, pack)
import Data.Tuple.Select (sel1, sel2, sel3)

type FileName = String

data FileTree = 
	File {name :: Text} 
	| Dir {name :: Text, contents :: [FileTree] } 
	| Failed {name :: Text} deriving Show

main = do
	args <- getArgs
	path <- (\x -> if (length x > 0) then return (head x) else getLine) args
	list <- Dir (pack path) <$> getSubDirContents path
	--putStrLn (show list)
	putStrLn "\n"
	performGC
	directoryAndFilesCount <- return $ countFilesAndDirectories list
	putStrLn ("Directories "++ (show . sel2 $  directoryAndFilesCount))
	putStrLn ("Files " ++ (show . sel1 $  directoryAndFilesCount))
	putStrLn ("Permission errors " ++ (show . sel3 $  directoryAndFilesCount))
	input <- getLine
	putStrLn input

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
tryReadContent path = handle possibleError $! getDirectoryContents path
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

