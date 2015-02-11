{
module Lexer
  (
    scanner
  , AlexPosn(..)
  , Token(..)
  , TokenClass(..)
  ) where
}

%wrapper "posn"

$digit      = 0-9
$alpha      = [a-zA-Z]
$whitespace = [\ \t]

@identifier = $alpha($alpha|$digit)*

tokens :-

$whitespace                       ;
\\                                { \pos -> \s -> Lambda pos LambdaC }
@identifier                       { \pos -> \s -> Ident pos s IdentC }
\.                                { \pos -> \s -> Dot pos DotC       }
\(                                { \pos -> \s -> LParen pos LParenC }
\)                                { \pos -> \s -> RParen pos RParenC }

{

data TokenClass = LambdaC | DotC | IdentC | LParenC | RParenC | EofC
                deriving (Show, Eq)

data Token = Lambda AlexPosn TokenClass
           | Dot AlexPosn TokenClass
           | Ident AlexPosn String TokenClass
           | LParen AlexPosn TokenClass
           | RParen AlexPosn TokenClass
           | EOF AlexPosn TokenClass
           deriving (Show, Eq)

alexEOF pos s = EOF pos

scanner :: String -> [Token]
scanner str = alexScanTokens str ++ [EOF (AlexPn 0 0 0) EofC]

}
