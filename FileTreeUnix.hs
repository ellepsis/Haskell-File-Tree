module FileTree where

import System.Directory 
import System.IO
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
import Control.Applicative
import Control.Monad
import Control.Exception as E
import Data.Binary
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)

type FileName = String

data FileTree = 
    File {name :: Text} 
    | Dir {name :: Text, contents :: [FileTree] } 
    | Failed {name :: Text} deriving (Eq)

instance Show FileTree where
    show tree = unlines $ showWithLevel 0 tree 
        where
            showWithLevel :: Int -> FileTree -> [String]
            showWithLevel level dir@(Dir name contents) = 
                (replicate level '-' ++ (showTreeName dir)) : (join $ map (showWithLevel (level+1)) contents)
            showWithLevel level file@(File name) = [replicate level '-' ++ (showTreeName file)]
            showWithLevel level failed@(Failed name) = [replicate level '-' ++ (showTreeName failed)]

instance Binary FileTree where
    put (File name)            = do 
                            put (0 :: Word8)
                            put (unpack name)
    put (Dir name contents)    = do 
                            put (1 :: Word8)
                            put (unpack name)
                            put contents
    put (Failed name)         = do
                            put (2 :: Word8)
                            put (unpack name)

    get = do 
        t <- (get :: Get Word8) 
        case t of 
            0 -> do
                name <- get
                return(File . pack $ name) 
            1 -> do
                name <- get 
                contents <- get
                return (Dir (pack name) contents)
            2 -> do 
                name <- get 
                return (Failed . pack $ name)

-- Current directory -> File path
getSubDirContents :: FilePath -> IO [FileTree]
getSubDirContents dirPath = do
    contents <- filter (\x -> x /= "." && x /= ".." ) <$> tryReadContent dirPath
    if (contents == ["\NUL"]) then return $ errorPathFileTree dirPath
        else do
            isDirs <-  sequence $ map
                      	(\x -> liftA2 (&&) (doesDirectoryExist x) (not <$> (isSymbolicLink <$> (getSymbolicLinkStatus x)))) 
				$ map (dirPath++) $ map ('/':) contents
            createTree dirPath contents isDirs                
    
-- Current directory -> Directory Contents -> Is Directory
createTree :: FilePath -> [FilePath] -> [Bool] -> IO [FileTree]
createTree currentDir filePaths isDir = do
    a <- sequence( zipWith (\x y -> 
        if (x==True) 
            then ( do 
                subDirContents <- getSubDirContents (currentDir++('/' : y))
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

checkIsDir :: FilePath -> IO Bool
checkIsDir path = liftA2 (&&) (doesDirectoryExist path) (possibleError (try $ getSymbolicLinkStatus path))
	where
	possibleError some  = do
		input <- some 
		case input of 
			Right info ->  return $ not (isSymbolicLink info) 
			Left (SomeException _) ->  return False

checkFail :: [FilePath] -> Bool
checkFail [] = False
checkFail list = last (head list) == '\NUL'

-- Return all posible non error paths to files from a tree
allFilePath :: FileTree -> [FilePath]
allFilePath (Dir name contents) = map (((unpack name)++"/")++) (join (map allFilePath contents))
allFilePath (File name) = [unpack name] 
allFilePath (Failed _) = []

isDir :: FileTree -> Bool
isDir (Dir _ _) = True
isDir _ = False

isFile :: FileTree -> Bool
isFile (File _) = True
isFile _ = False

isFailed :: FileTree -> Bool
isFailed (Failed _) = True
isFailed _ = False

showTreeName :: FileTree -> String
showTreeName (Dir name _) = unpack name ++ "/"
showTreeName (File name) = unpack name
showTreeName (Failed name) = "***Failed: " ++ (unpack name)
