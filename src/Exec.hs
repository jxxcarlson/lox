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

import qualified TokenParser
import Scanner
import qualified Parser as EP -- Expressions Parser
import Eval(eval)
import qualified MiniParsec

-- DISPATCHER

data PState = PState { count :: Int
                     , message :: String
                     }

initialPState :: PState
initialPState = PState { count = 0, message = "" }

prefix = "-----\n"

exec :: PState -> String -> IO PState
exec pState line =
  return pState { message = runLine' (count pState) line }


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
        Data.List.intercalate "\n" $ map (\(k, input') -> runLine' k input') inputs

runLine :: Int -> String -> String
runLine k input_ =
    let
      input = trimLeadingSpaces $ input_ ++ " "
    in
    case Scanner.line k input of
        (_, Right tokens) -> prefixLine k (cyan input) ++ Scanner.prettyPrint tokens
        (remainder, Left error) -> prefixLine k (cyan input) ++ red (show error)

runLine' :: Int -> String -> String
runLine' k input_ =
    let
      input = trimLeadingSpaces $ input_ ++ " "
    in
    case Scanner.line k $ trimLeadingSpaces input of
        (_, Right tokens) -> -- prefixLine k (cyan input) ++ Scanner.prettyPrint tokens
           case MiniParsec.runParser EP.expression tokens of
               (_, Right e) -> prettyPrintExpression k input tokens e ++ magenta(" => ") ++ green (show (eval e)) 
               (ts', Left error') -> prettyprintError k input tokens error'
        (remainder, Left error) -> prefixLine k (cyan input) ++ red (show error)


prettyprintError :: Show a => Int -> String -> [Token] -> a -> String
prettyprintError k input tokens error' =
    prefixLine k (cyan input) ++ magenta (Scanner.prettyPrint tokens) ++ " : " ++ red (show error')

prettyPrintExpression :: Show a => Int -> String -> [Token] -> a -> String
prettyPrintExpression k input tokens expr =
    prefixLine k (cyan input)
    ++ " : " ++ magenta (Scanner.prettyPrint tokens)
    ++ " : " ++  show expr -- EP.prettyPrint expr


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
processInputLine (k, input) = putStrLn $ runLine' k input

isSpace :: Char -> Bool
isSpace c = c == ' '

trimLeadingSpaces :: String -> String
trimLeadingSpaces = dropWhile isSpace

removeComments :: [String] -> [String]
removeComments lines_ = filter (\l -> l !! 0 /= '#') $ filter (\l -> length l > 0) $ lines_

numberLines :: [String] -> [(Int, String)]
numberLines lines_ = zip [0..] lines_

prefixLine :: Int -> String -> String
prefixLine k input = "Line " ++ show k ++ ": " ++ input ++ " :: "