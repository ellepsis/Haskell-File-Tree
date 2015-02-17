module Main where

import FileTree
import TreeOperations
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt
import System.Exit
import Control.Applicative
import Control.Monad
import Data.List (delete, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M  
import Data.Text (Text, pack, unpack)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.String (unlines)
import Numeric

-- Supported options:
-- input style
-- ./tree [-scvh] directory path
-- Size:                         -s 
-- Size in human format         -h
-- Files and Directories count: -c 
-- No tree output                -v 

data Option = Size | SizeInHumanReadableFormat | Count | NoTreeOutput | StatisticExtension | DirectoryPath String deriving (Eq)

main =  do
    hSetEncoding stdout utf8
    args <- getArgs
    (options, paths) <- compilerOpts args
    when ((nub options) == [NoTreeOutput]) (exitWith $ ExitSuccess)
    path <- (\x -> if (length x > 0) then return (head x) else getLine) (paths)
    list <- Dir (pack $ init path) <$> getSubDirContents path
    str <- getLine
    list2 <- Dir (pack $ init path) <$> getSubDirContents path
    putStrLn $ show $ treeDifference list list2 path
    options' <- (\x -> if (elem NoTreeOutput x) then return $ delete NoTreeOutput (x) else (putStrLn (show list) >> return x )) (nub options)
    whatToDo options' list
    

makeFileTree :: FilePath -> IO FileTree
makeFileTree path = Dir (pack $ init path) <$> getSubDirContents path

whatToDo :: [Option] -> FileTree -> IO ()
whatToDo list tree
    | elem StatisticExtension list = do
        let stat = extentionCount tree
        let filesCount = sel1 $ countFilesAndDirectories tree
        putStrLn "Lal"
        putStrLn $ (\x -> show x ++ " Files without extension " ++ persent x filesCount) (dotCount stat)
        putStrLn $ unlines $ map (\(x,y) -> show y ++ "\t." ++ x ++ "\t\t" ++  persent y filesCount) (M.assocs stat)
        whatToDo (delete StatisticExtension list) tree
    | elem Size list = do 
        dirSize <- directorySize "" tree
        putStrLn (show (length . sel2 $ dirSize) ++" Errors :" ++ (unlines . sel2 $ dirSize))
        list' <- if (elem SizeInHumanReadableFormat list) 
            then (putStrLn ("Size " ++ (printSizeInHumanReadableFormat . sel1 $ dirSize)) >> return (delete SizeInHumanReadableFormat list))
            else (putStrLn ("Size " ++ (show . sel1 $ dirSize)) >> return list)
        whatToDo (delete Size list) tree
    | elem Count list = do
        directoryAndFilesCount <- return $ countFilesAndDirectories tree
        putStrLn ("Directories "++ (show . sel2 $  directoryAndFilesCount))
        putStrLn ("Files " ++ (show . sel1 $  directoryAndFilesCount))
        putStrLn ("Permission errors " ++ (show . sel3 $  directoryAndFilesCount))
        whatToDo (delete Count list) tree
    | otherwise = return ()
    where
        dotCount :: M.Map String Int -> Int
        dotCount stat = fromMaybe 0 $ M.lookup "." stat
        persent ::  Int -> Int -> String
        persent a b = showGFloat (Just 2) (100*(fromIntegral(a)/(fromIntegral b))) "%"

options :: [OptDescr Option]
options =
    [ Option ['s']     ["size"]         (NoArg Size)                            "Show size"
    , Option ['h']     ["human"]        (NoArg SizeInHumanReadableFormat)       "Show size in human readable format"
    , Option ['c']     ["count"]        (NoArg Count)                           "Files and Directories count"
    , Option ['v']     ["noTreeOutput"] (NoArg NoTreeOutput)                    "Suppress tree display"
    , Option ['e']     ["statistic"]    (NoArg StatisticExtension)               "Show statistic extension"
    ] 

compilerOpts :: [String] -> IO ([Option], [String])
compilerOpts argv = 
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (o,n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: ic [OPTION...] files..."