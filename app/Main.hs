import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec



main :: IO()
main =
  do
    putStrLn "\nWelcome to Lox"
    putStr "Run file ? [filename|RETURN] > " >> hFlush stdout
    line <- getLine
    let args = words line
    if length args > 1 then
        putStrLn "Usage: jlox [script]"
    else if length args == 1 then 
        runFile (args !! 0)
    else repl


repl :: IO()
repl =
  do
    let pState = initialPState
    putStrLn "\nHello!  Type /quit to quit, /help for help\n"
    loop pState

loop :: PState -> IO ()
loop pState = do
  putStr (show (count pState) ++ ": lox > " ) >> hFlush stdout  
  line <- getLine
  pState' <- exec pState line
  unless (line == ":quit") $ putStrLn (message pState') >> loop pState' {count = count pState' + 1}


