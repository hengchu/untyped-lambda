module Pretty
  (
    prettyprint
  )
  where

import Trans (NLTerm(..))

prettyprint :: NLTerm -> IO ()
prettyprint t = case t of
                  NLVar x -> do putStr $ "x" ++ show x
                  NLAbs x term -> do putStr $ "λ x" ++ show x ++ ". "
                                     prettyprint term
                  NLApp t1 t2 -> do putStr "("
                                    prettyprint t1
                                    putStr ") "
                                    putStr "("
                                    prettyprint t2
                                    putStr ") "
