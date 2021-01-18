{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment

import Data.List.NonEmpty (fromList)

-- sendGridApiKey :: ApiKey
-- sendGridApiKey = ApiKey "SG..."

 
-- DISPATCHER

data PState = PState { count :: Int
                     , message :: String
                     }

initialPState :: PState
initialPState = PState { count = 0, message = "" }

prefix = "-----\n"

exec :: PState -> String -> IO PState
exec pState str = 
  case words str of
     [] -> return pState { message = ""}
     [":help"]  -> help pState
     args ->  return pState {message = prefix ++  "I don't understand\n" ++ str }



help :: PState -> IO PState
help pState = do
  text <- TIO.readFile  "help.txt"
  return pState { message = T.unpack text}

run :: String -> String
run input = "Not implemented yet (run)"


runFile :: String -> IO ()
runFile filePath =
    do
    input <- TIO.readFile filePath 
    putStrLn $ run $ T.unpack input
