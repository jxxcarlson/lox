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
  putStr (show (count pState) ++ ": info > " ) >> hFlush stdout  
  input <- innerLoop "" 
  pState' <- exec pState input
  unless (input == "/quit ") $ putStrLn (message pState') >> loop pState' {count = count pState' + 1}


innerLoop :: String -> IO String
innerLoop str = do
   line <- getLine
   if line == "" then return ""
   else if head line == '/' then 
       return (line ++ " " ++ str)
   else  
       innerLoop (str ++ "\n" ++ line)

