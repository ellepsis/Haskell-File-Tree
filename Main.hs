module Main where

import FileTree
import TreeOperations
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt
import System.Exit
import System.Directory 
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.List (delete, nub)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as M  
import Data.Text (Text, pack, unpack)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.String (unlines)
import Numeric

-- Supported options:
-- input style
-- ./tree [-d] [path] [-scvh] directory path
-- Size:                        -s 
-- Size in human format         -h
-- Files and Directories count: -c 
-- No tree output               -v 
-- Write tree                   -w
-- Load tree                    -l
-- Extention statistic          -e
-- Difference between two trees -

data Option = Size | SizeInHumanReadableFormat | Count | NoTreeOutput | StatisticExtension | DifferenceArg (Maybe String) | Write String | Load String deriving (Eq)

-- Create tree and execute options
main =  do
    hSetEncoding stdout utf8
    args <- getArgs
    (options, paths) <- compilerOpts args  
    path <- (\x -> if (length x > 0) then return $ head x else getLine) (paths)
    putStrLn "Please wait... Construct tree"
    list <- if (length (filter isLoad $ nub options) > 0)
        -- if Load option given then load tree from file
        then localLoadTree $ head $ filter isLoad $ nub options
        else makeFileTree path
    putStrLn "Complite"
    options' <- (\x -> if (elem NoTreeOutput x) then return $ delete NoTreeOutput (x) else (putStrLn (show list) >> return x )) (nub options)
    whatToDo options' list path
    where
        localLoadTree :: Option -> IO FileTree
        localLoadTree (Load path) = do
            otherTree <- loadTree path
            case otherTree of Just tree -> return tree
                              Nothing -> (putStrLn "File isn't a tree or damaged") >> exitFailure        
        isLoad :: Option -> Bool
        isLoad (Load _) = True
        isLoad _ = False

loadTree :: FilePath -> IO (Maybe FileTree)
loadTree path = do
    fileExist <- doesFileExist path
    if (fileExist) 
        then do 
            tree <- E.handle possibleError $ decodeFile path 
            if (tree == (Failed (pack "")))
                then return Nothing
                else return $ Just tree
        else (putStrLn "File dont exist") >> exitFailure
    where
        possibleError :: SomeException -> IO FileTree
        possibleError _ = return $ Failed $ pack ""

-- Create file tree from path
makeFileTree :: FilePath -> IO FileTree
makeFileTree path = Dir (pack $ init path) <$> getSubDirContents path

-- Execute options
whatToDo :: [Option] -> FileTree -> FilePath -> IO ()
whatToDo list tree inputPath
    -- Difference
    | length (filter isDifferenceArg list) > 0 = do
        let diffArg = head $ filter (isDifferenceArg) list
        otherTree <- case diffArg of 
            (DifferenceArg Nothing) -> do 
                putStrLn "Press any key when changes will be completed..." 
                getChar 
                localTree <- Dir (pack $ init inputPath) <$> getSubDirContents inputPath
                return (Just localTree)
            (DifferenceArg (Just path)) -> do 
                putStrLn "Load other tree"
                loadTree path
        case otherTree of Nothing -> (putStrLn "File isn't a tree or damaged") >> whatToDo (delete diffArg list) tree inputPath
                          Just tree2 ->
                             (putStrLn $ unlines $ map show $ treeDifference tree tree2 inputPath) >>
                            whatToDo (delete diffArg list) tree inputPath
    -- Write
    | length (filter isWriteArg list) > 0 = do
        let (Write path) = head $ filter (isWriteArg) list
        E.handle errorWriteTree (encodeFile path tree)
        encodeFile path tree
    -- Extension statistic
    | elem StatisticExtension list = do
        let stat = extentionCount tree
        let filesCount = sel1 $ countFilesAndDirectories tree
        putStrLn $ (\x -> show x ++ " Files without extension " ++ persent x filesCount) (dotCount stat)
        putStrLn $ unlines $ map (\(x,y) -> show y ++ "\t." ++ x ++ "\t\t" ++  persent y filesCount) (M.assocs stat)
        whatToDo (delete StatisticExtension list) tree inputPath
    -- Files size
    | elem Size list = do 
        dirSize <- directorySize "" tree
        putStrLn (show (length . sel2 $ dirSize) ++" Errors :" ++ (unlines . sel2 $ dirSize))
        list' <- if (elem SizeInHumanReadableFormat list) 
            then (putStrLn ("Size " ++ (printSizeInHumanReadableFormat . sel1 $ dirSize)) >> return (delete SizeInHumanReadableFormat list))
            else (putStrLn ("Size " ++ (show . sel1 $ dirSize)) >> return list)
        whatToDo (delete Size list) tree inputPath
    -- Files count
    | elem Count list = do
        directoryAndFilesCount <- return $ countFilesAndDirectories tree
        putStrLn ("Directories "++ (show . sel2 $  directoryAndFilesCount))
        putStrLn ("Files " ++ (show . sel1 $  directoryAndFilesCount))
        putStrLn ("Permission errors " ++ (show . sel3 $  directoryAndFilesCount))
        whatToDo (delete Count list) tree inputPath
    | otherwise = return ()
    where
        dotCount :: M.Map String Int -> Int
        dotCount stat = fromMaybe 0 $ M.lookup "." stat
        isDifferenceArg :: Option -> Bool
        isDifferenceArg (DifferenceArg _) = True
        isDifferenceArg _ = False 
        isWriteArg :: Option -> Bool
        isWriteArg (Write _) = True
        isWriteArg _ = False
        errorWriteTree :: SomeException -> IO ()
        errorWriteTree _ = putStrLn "Error write tree" 
        persent ::  Int -> Int -> String
        persent a b = showGFloat (Just 2) (100*(fromIntegral(a)/(fromIntegral b))) "%"

options :: [OptDescr Option]
options =
    [ Option ['s']     ["size"]         (NoArg Size)                            "Show size"
    , Option ['h']     ["human"]        (NoArg SizeInHumanReadableFormat)       "Show size in human readable format"
    , Option ['c']     ["count"]        (NoArg Count)                           "Files and Directories count"
    , Option ['v']     ["noTreeOutput"] (NoArg NoTreeOutput)                    "Suppress tree display"
    , Option ['e']     ["statistic"]    (NoArg StatisticExtension)              "Show statistic extension"
    , Option ['d']     ["difference"]   (OptArg makeArg "File")                 "Show difference between two trees"
    , Option ['w']     ["write"]        (ReqArg Write "File")                   "Write tree to file"
    , Option ['l']     ["load"]         (ReqArg Load "File")                    "Load tree from file"
    ] 

makeArg :: Maybe String -> Option
makeArg arg = DifferenceArg arg

compilerOpts :: [String] -> IO ([Option], [String])
compilerOpts argv = 
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (o,n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: ic [OPTION...] files..."