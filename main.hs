import Parser
import Trans
import Eval
import Pretty
import GHC.IO.Handle
import GHC.IO.Handle.FD

loop :: IO ()
loop = do putStr "> "
          hFlush stdout
          input <- getLine
          case parseInput "<stdin>" input of
            Left  _  -> return ()
            Right toks -> prettyprint $ eval $ fst $ trans toks
          putStr "\n"
          hFlush stdout
          loop

main :: IO ()
main = loop
