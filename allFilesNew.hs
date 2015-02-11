module Main where

import System.Directory
import Data.List
import Data.List.Split
import Prelude
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent

type FileName = String

data FileTree = 
	File {name :: FilePath} 
	| Dir {name :: FilePath, contents :: [FileTree] } 
	| Failed {name :: FilePath} deriving Show

main = do
	path <- getLine
	--path <- return "C:\\Leksah\\"
	list <- getSubDirContents path
	putStrLn (show list)

getSubDirContents :: FilePath -> IO [FileTree]
getSubDirContents dirPath = do
	contents <- map (dirPath ++) <$> (map ('/' :) <$> (filter (\x -> x /= "." && x /= ".." ) <$> tryReadContent dirPath))
	if (checkFail contents) then return $ errorPathFileTree dirPath
		else do
			isDirs <-  sequence $ map doesDirectoryExist contents
			createTree contents isDirs				
	

createTree :: [FilePath] -> [Bool] -> IO [FileTree]
createTree filePaths isDir = do
	a <- sequence( zipWith (\x y -> 
		if (x==True) 
			then ( do 
				subDirContents <- getSubDirContents y
				return $! Dir y subDirContents )
			else return (File y)) isDir filePaths)
	return a

fileNameFromPath :: FilePath -> FileName
fileNameFromPath fp = last (splitOn "/" fp)

errorPathFileTree :: FilePath -> [FileTree]
errorPathFileTree path = [Failed path]

tryReadContent :: FilePath -> IO [FilePath]
tryReadContent path = handle possibleError (getDirectoryContents path)
	where 
		possibleError :: IOException -> IO [FilePath]
		possibleError _ = return ["\NUL"]

checkFail :: [FilePath] -> Bool
checkFail [] = False
checkFail list = last (head list) == '\NUL'