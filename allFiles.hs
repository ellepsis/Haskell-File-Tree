module Main where

import System.Directory
import Data.List
import Data.List.Split
import Prelude
import Control.Applicative
import Control.Monad

type FileName = String

data FileTree = 
	File {name :: FileName} 
	| Dir {name :: FileName, contents :: [FileTree] } 
	| Failed {name :: FileName} deriving Show

data PathFileTree = PathFileTree {path :: FilePath, pathFileTree :: [PathFileTree], fileTree :: FileTree} deriving Show

main = do
	path <- getLine
	list <- getSubDirContents path
	putStrLn (show list)

getSubDirContents :: FilePath -> IO [PathFileTree]
getSubDirContents dirPath = do
	contents <- map (dirPath ++) <$> (filter (\x -> x /= "." && x /= ".." ) <$> tryReadContent dirPath)
	if (contents == ["\NUL"]) then return $ errorPathFileTree dirPath
		else do
			isDirs <-  sequence $ map doesDirectoryExist contents
			createTree contents isDirs				
	

createTree :: [FilePath] -> [Bool] -> IO [PathFileTree]
createTree filePaths isDir = do
	a <- sequence( zipWith (\x y -> 
		if (x==True) 
			then ( do 
				subDirContents <- getSubDirContents y
				return $ PathFileTree y subDirContents $ Dir (fileNameFromPath y) (map fileTree subDirContents)) --Need add a condition that make end if returned [] PathFileTree
			else (return $ PathFileTree y [] $ (File . fileNameFromPath)  ('/':y))) isDir filePaths)
	return a

fileNameFromPath :: FilePath -> FileName
fileNameFromPath fp = last (splitOn "/" fp)

errorPathFileTree :: FilePath -> [PathFileTree]
errorPathFileTree path = [PathFileTree path [] (Failed $ fileNameFromPath path)]

tryReadContent :: FilePath -> IO [FilePath]
tryReadContent path = handle possibleError (getDirectoryContents path)
	where 
		possibleError :: IOException -> IO [FilePath]
		possibleError _ = return ["\NUL"]