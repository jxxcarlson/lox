{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment

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
  return pState { message = show $ Scanner.parseLine (count pState) line }


help :: PState -> IO PState
help pState = do
  text <- TIO.readFile  "help.txt"
  return pState { message = T.unpack text}

run :: String -> String
run input = "not yet implemented"

runLine :: Int -> String -> String 
runLine k input = 
    case Parser.runParser (Scanner.lineParser k) input of 
        ("", Right tokens) -> show tokens
        (remainder, Left error) -> show error
    


runFile :: String -> IO ()
runFile filePath =
    do
    input <- TIO.readFile filePath 
    putStrLn $ run $ T.unpack input


