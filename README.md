# untyped-lambda
A small untyped lambda calculus interpreter

----

This is a small interpreter for untyped lambda calculus. It is implemented as an exercise after reading Chap 5 and 6 of Types and Programming Languages (http://www.cis.upenn.edu/~bcpierce/tapl/). I implemented De Bruijn Indices mentioned in the book to avoid name capture issues in substitutions. I also found De Bruijn's original paper from 1972 on this matter really helpful in understanding this method.

To run it, first `alex lexer.x`, then `ghc main.hs` and finally `./main`. You would need the `alex` (https://www.haskell.org/alex/) tool to generate the lexer.

Criticisms are welcome! Please let me know if anything looks wrong :)
