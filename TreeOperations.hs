module TreeOperations where

import FileTree
import System.IO
import Control.Applicative
import Control.Monad
import Control.Exception as E
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)

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

pritSizeInHumanReadableFormat :: Integer -> String
pritSizeInHumanReadableFormat bytes 
	| quot bytes (2^40) > 0 = (show $ quot bytes (2^40)) ++ " TiB " ++ (pritSizeInHumanReadableFormat $ mod bytes (2^40))
	| quot bytes (2^30) > 0 = (show $ quot bytes (2^30)) ++ " GiB " ++ (pritSizeInHumanReadableFormat $ mod bytes (2^30))
	| quot bytes (2^20) > 0 = (show $ quot bytes (2^20)) ++ " MiB " ++ (pritSizeInHumanReadableFormat $ mod bytes (2^20))
	| quot bytes (2^10) > 0 = (show $ quot bytes (2^10)) ++ " KiB " ++ (pritSizeInHumanReadableFormat $ mod bytes (2^10))
	| otherwise = (show bytes) ++ " Bytes " 