import Parser
import Trans
import Eval
import Pretty
import Control.Exception
import GHC.IO.Handle
import GHC.IO.Handle.FD

loop :: IO ()
loop = do putStr "> "
          hFlush stdout
          ineof <- isEOF
          if not ineof then
            do input <- getLine
               case parseInput "<stdin>" input of
                 Left  err  -> error $ show err
                 Right toks -> prettyprint $ eval $ fst $ trans toks
               putStr "\n"
               hFlush stdout
               loop
          else
            do putStrLn ""
               return ()

loop' :: IO ()
loop' = loop `catch` (\e -> do {print (e::Control.Exception.SomeException); loop'})

main :: IO ()
main = loop' 
