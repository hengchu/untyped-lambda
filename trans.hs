-- This module implements a function that translates
-- named lambda calculus to a nameless one using de
-- Bruijn indices

module Trans
  (
    trans
  , NLTerm(..)
  )
  where

import Parser (Term(..))
import Data.List
import Data.Maybe

data NLTerm = NLVar Int
            | NLAbs Int NLTerm
            | NLApp NLTerm NLTerm
            deriving (Show, Eq)

type Context = [String]

trans :: Term -> (NLTerm, Context)
trans = trans' []

trans' :: Context -> Term -> (NLTerm, Context)
trans' ctx (TmVar x) = case x `elemIndex` ctx of
                         Just idx -> (NLVar idx, ctx)
                         Nothing  -> error $ "Variable: " ++ x ++ " is unbound"

trans' ctx (TmAbs x t) = let ctx' = x:ctx
                             (t', ctx'') = trans' ctx' t
                             x' = fromJust $ x `elemIndex` ctx''
                         in  (NLAbs x' t', ctx'')

trans' ctx (TmApp t1 t2) = let (t1', _) = trans' ctx t1
                               (t2', _) = trans' ctx t2
                           in (NLApp t1' t2', ctx)
