module Parser
  (
    Term(..)
  , parseInput
  ) where

import Lexer (scanner, Token(..), TokenClass(..), AlexPosn(..))
import Text.Parsec
import Control.Monad

data Term = TmVar String
          | TmAbs String Term
          | TmApp Term   Term
          deriving (Show, Eq)

type LParser b = Parsec [Token] () b

token2pos :: Token -> AlexPosn
token2pos (Dot pos     _ ) = pos
token2pos (Lambda pos  _ ) = pos
token2pos (LParen pos  _ ) = pos
token2pos (RParen pos  _ ) = pos
token2pos (Ident pos _ _ ) = pos
token2pos (EOF pos _ )     = pos

token2tokenclass :: Token -> TokenClass
token2tokenclass (Dot _ c)     = c
token2tokenclass (Lambda _ c)  = c
token2tokenclass (LParen _ c)  = c
token2tokenclass (RParen _ c)  = c
token2tokenclass (Ident _ _ c) = c
token2tokenclass (EOF _ c)     = c

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos t _ = let AlexPn _ line col = token2pos t
                    in setSourceLine (setSourceColumn pos col) line

pSimpleToken :: TokenClass -> LParser ()
pSimpleToken t = tokenPrim show updatePos acceptTok >> return ()
  where acceptTok a | token2tokenclass a == t = Just a
                    | otherwise               = Nothing

pId :: LParser String
pId = tokenPrim show updatePos acceptTok
  where acceptTok (Ident _ str _) = Just str
        acceptTok _               = Nothing

pVar :: LParser Term
pVar = liftM TmVar pId

pAbs :: LParser Term
pAbs = do pSimpleToken LambdaC
          v <- pId
          pSimpleToken DotC
          t <- term
          return $ TmAbs v t

pAppTerm :: LParser Term
pAppTerm = do ts <- pAppTerm'
              return $ mkApp (head ts) $ tail ts
  where mkApp t [] = t
        mkApp t (x:xs) = mkApp (TmApp t x) xs

pAppTerm' :: LParser [Term]
pAppTerm' = do t <- pAterm
               rest <- optionMaybe pAppTerm'
               case rest of
                 Nothing -> return [t]
                 Just ts -> return $ t:ts

pAterm :: LParser Term
pAterm = try $ between (pSimpleToken LParenC) (pSimpleToken RParenC) term <|> pVar

term :: LParser Term
term = try pAppTerm <|> pAbs

parser :: LParser Term
parser = do t <- term
            pSimpleToken EofC
            return t

parseInput :: String -> String -> Either ParseError Term
parseInput srcname input = let toks = scanner input 
                           in parse parser srcname toks
