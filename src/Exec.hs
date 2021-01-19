{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment
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
    let 
        prefix k  = "Line " ++ show k ++ ": " ++ input ++ " :: " 
    in
    case Scanner.line k input of 
        ("", Right tokens) -> prefix k ++ Scanner.prettyPrint tokens
        (remainder, Left error) -> prefix k ++  show error
        _ -> prefix k ++ colorRed "Unexplained error"
    

colorRed :: String -> String
colorRed s = "\"\\u001b[31m" ++ s ++ "\\e[0m\""

runFile :: String -> IO ()
runFile filePath = 
    do
    input <- TIO.readFile filePath 
    putStrLn $ run $ T.unpack input


