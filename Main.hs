module Main where

import FileTree
import TreeOperations
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt
import Control.Applicative
import Data.Text (Text, pack, unpack)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.String (unlines)

-- Supported options:
-- input style
-- ./tree [-scv] directory path
-- Size: 						-s 
-- Files and Directories count: -c 
-- No tree output				-v 

data Options = Options
    { optSize      		:: Bool
    , optCount 	   		:: Bool
    , optNoTreeOutput 	:: Bool
    , optDirs   		:: [FilePath]
    } deriving Show

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
	putStrLn ("Size " ++ (pritSizeInHumanReadableFormat . sel1 $ dirSize))
	putStrLn (show (length . sel2 $ dirSize) ++" Errors :" ++ (unlines . sel2 $ dirSize))

makeFileTree :: FilePath -> IO FileTree
makeFileTree path = Dir (pack $ init path) <$> getSubDirContents path

defaultOptions    = Options
	{ optSize      		= false
    , optCount 	   		= false
    , optNoTreeOutput 	= false
    , optDirs   		= []
    } 

options :: [OptDescr Flag]
   options =
    [ Option ['s']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
    , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
    , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
    , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
    , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
    ]