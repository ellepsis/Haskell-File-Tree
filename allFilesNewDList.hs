module Main where

import System.Directory
import Data.List
import Data.List.Split
import Prelude
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.DList

type FileName = String
type FilePathD = DList Char

data FileTree = 
	File {name :: FilePathD} 
	| Dir {name :: FilePathD, contents :: [FileTree] } 
	| Failed {name :: FilePathD} deriving Show

main = do
	path <- getLine
	--path <- return "C:\\Leksah\\"
	list <- getSubDirContents (fromList path)
	putStrLn (show list)

getSubDirContents :: FilePathD -> IO [FileTree]
getSubDirContents dirPath = do
	contents <- Prelude.map (append dirPath) <$> ((Prelude.map fromList) <$> ((filter (\x -> x /= "." && x /= ".." ) <$> (tryReadContent dirPath))))
	if (Prelude.map toList contents == ["\NUL"]) then return $ errorPathFileTree dirPath
		else do
			isDirs <-  sequence $ Prelude.map doesDirectoryExist $ Prelude.map toList contents
			createTree contents isDirs				
	

createTree :: [FilePathD] -> [Bool] -> IO [FileTree]
createTree filePaths isDir = do
	a <- sequence( zipWith (\x y -> 
		if (x==True) 
			then ( do 
				subDirContents <- getSubDirContents y
				return $! Dir y subDirContents )
			else return (File y)) isDir filePaths)
	return a

fileNameFromPath :: FilePathD -> FileName
fileNameFromPath fp = last (splitOn "/" $ toList fp)

errorPathFileTree :: FilePathD -> [FileTree]
errorPathFileTree path = [Failed path]

tryReadContent :: FilePathD -> IO [FilePath]
tryReadContent path = handle possibleError $ getDirectoryContents $ toList path
	where 
		possibleError :: IOException -> IO [FilePath]
		possibleError _ = return ["\NUL"]