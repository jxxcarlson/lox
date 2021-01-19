{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment
import qualified System.Directory.Internal as SysD
import Data.List

import Data.List.NonEmpty (fromList)

import Parser
import Scanner
 
-- DISPATCHER

data PState = PState { count :: Int
                     , message :: String
                     }

initialPState :: PState
initialPState = PState { count = 0, message = "" }

prefix = "-----\n"

exec :: PState -> String -> IO PState
exec pState line = 
  return pState { message = runLine (count pState) line }


help :: PState -> IO PState
help pState = do
  text <- TIO.readFile  "help.txt"
  return pState { message = T.unpack text}

run :: String -> String
run input = 
    let 
        lines_ = filter (\l -> l !! 0 /= '#') $ filter (\l -> length l > 0) $ lines input
        inputs = zip [0..] lines_
    in
        Data.List.intercalate "\n" $ map (\(k, input') -> runLine k input') inputs

runLine :: Int -> String -> String
runLine k input = 
    case Scanner.line k $ trimLeadingSpaces input of 
        ("", Right tokens) -> prefixLine k (cyan input) ++ Scanner.prettyPrint tokens
        (remainder, Left error) -> prefixLine k (cyan input) ++ red (show error)
        _ -> prefixLine k (cyan input) ++ red "Unexplained error"


black :: String -> String
black str = "\x1b[30m" ++ str ++ "\x1b[0m"

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

green :: String -> String
green str = "\x1b[32m" ++ str ++ "\x1b[0m"

yellow :: String -> String
yellow str = "\x1b[33m" ++ str ++ "\x1b[0m"

blue :: String -> String
blue str = "\x1b[34m" ++ str ++ "\x1b[0m"

magenta :: String -> String
magenta str = "\x1b[35m" ++ str ++ "\x1b[0m"

cyan :: String -> String
cyan s = "\x1b[36m" ++ s ++ "\x1b[0m"

white :: String -> String
white s = "\x1b[37m" ++ s ++ "\x1b[0m"

runFile :: String -> IO ()
runFile filePath = 
    do
    input <- TIO.readFile filePath 
    let inputLines = numberLines . removeComments . lines . T.unpack $ input
    SysD.sequenceWithIOErrors_ $ processInputLines inputLines


processInputLines :: [(Int, String)] -> [IO()]
processInputLines lines_ = map processInputLine lines_

processInputLine :: (Int, String) -> IO()
processInputLine (k, input) = putStrLn $ runLine k input

trimLeadingSpaces :: String -> String 
trimLeadingSpaces = dropWhile isSpace

removeComments :: [String] -> [String]
removeComments lines_ = filter (\l -> l !! 0 /= '#') $ filter (\l -> length l > 0) $ lines_

numberLines :: [String] -> [(Int, String)]
numberLines lines_ = zip [0..] lines_

prefixLine :: Int -> String -> String 
prefixLine k input = "Line " ++ show k ++ ": " ++ input ++ " :: " 