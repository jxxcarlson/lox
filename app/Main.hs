import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec



main :: IO()
main =
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


