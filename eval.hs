module Eval
  (
    eval
  )
  where

import Trans (NLTerm(..))

shift :: Int -> Int -> NLTerm -> NLTerm
shift d c t = case t of
                NLVar k -> NLVar $ if k < c then k else k+d
                NLAbs k  term -> NLAbs k $ shift d (c+1) term
                NLApp t1 t2   -> NLApp (shift d c t1) (shift d c t2)

shiftBase :: Int -> NLTerm -> NLTerm
shiftBase d = shift d 0

isval :: NLTerm -> Bool
isval (NLAbs _ _) = True
isval _           = False

termSubst :: Int -> NLTerm -> NLTerm -> NLTerm
termSubst j s t = case t of
                    NLVar k -> if k == j then s else (NLVar k)
                    NLAbs k term -> NLAbs k $ termSubst (j+1) (shiftBase 1 s) term
                    NLApp t1 t2 -> NLApp (termSubst j s t1) (termSubst j s t2)

termSubstTop :: NLTerm -> NLTerm -> NLTerm
termSubstTop s t = shiftBase (-1) $ termSubst 0 (shiftBase 1 s) t

eval1 :: NLTerm -> NLTerm
eval1 t = case t of
            NLApp (NLAbs _ t12) v2 | isval v2 -> termSubstTop v2 t12
            NLApp v1 t2 | isval v1 -> let t2' = eval1 t2
                                      in  NLApp v1 t2'
            NLApp t1 t2 -> let t1' = eval1 t1
                           in  NLApp t1' t2
            _ -> t

eval :: NLTerm -> NLTerm
eval t = let t' = eval1 t
         in  if t' == t then t -- no more reduction possible
             else eval t'
