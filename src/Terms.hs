module Terms where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative ((<$>))
import qualified Data.Map as M
import qualified Data.Set as S

import Text.ParserCombinators.Parsec.Expr (Assoc(AssocLeft, AssocRight))

import TRS
import TRSParser

{- Term Rewriting -}

isWellformed :: Signature -> Term -> Bool
isWellformed a (Var _) = True
isWellformed a (Fun f args) = M.lookup f a == Just (genericLength args) &&
                                  all (isWellformed a) args

applySubst :: Subst -> Term -> Term
applySubst σ (Var x) = maybe (Var x) id (M.lookup x σ)
applySubst σ (Fun f fargs) = Fun f (map (applySubst σ) fargs)

isGround :: Term -> Bool
isGround (Var _) = False
isGround (Fun _ args) = all isGround args

match :: (Term, Term) -> Maybe Subst
match p = match' p M.empty
    where match' (Fun f fArgs, Fun g gArgs) σ
            | f /= g || genericLength fArgs /= genericLength gArgs = Nothing
            | otherwise = foldl (\σ t1t2 -> σ >>= match' t1t2) (Just σ) (zip fArgs gArgs)
          match' (t, Var x) σ = 
            case M.lookup x σ of
              Nothing -> Just (M.insert x t σ)
              Just t' -> if t == t' then Just σ else Nothing
          match' _ _ = Nothing

rewriteOneWith :: Rule -> [Term] -> [[Term]]
rewriteOneWith rule ts = snd (foldr f ([], []) ts)
    where f t (ts, acc) = (t:ts, map (:ts) (rewriteWith rule t) ++ map (t:) acc)

rewriteWith :: Rule -> Term -> [Term]
rewriteWith rule@(Rule l r) t = 
   maybeToList ((\σ -> applySubst σ r) <$> match (t, l)) ++
   case t of
       Var x -> []
       Fun f args -> map (Fun f) (rewriteOneWith rule args)
   
rewrite :: [Rule] -> Term -> [Term]
rewrite rules t = rules >>= (\rule -> rewriteWith rule t)

rewriteStar :: [Rule] -> Term -> [Term]
rewriteStar rules t = S.toList $ rewriteStar' (S.singleton t)
   where rewriteStar' ts = 
             case S.fold f (False, S.empty) ts of
                 (True, ts') -> rewriteStar' ts'
                 (False, ts') -> ts'
         f t (b,ts') = 
             case rewrite rules t of
                 [] -> (b, S.insert t ts')
                 ts'' -> (True, foldl (\ts' t -> S.insert t ts') ts' ts'')
                 
rewriteTrace :: [Rule] -> Term -> [Trace]
rewriteTrace rules t = S.toList $ rewriteTrace' (S.singleton (Trace [t]))
   where rewriteTrace' ts = 
             case S.fold f (False, S.empty) ts of
                 (True, ts') -> rewriteTrace' ts'
                 (False, ts') -> ts'
         f (Trace tr) (b,ts') = 
             case rewrite rules (head tr) of
                 [] -> (b, S.insert (Trace tr) ts')
                 ts'' -> (True, foldl (\ts' t' -> 
                     S.insert (Trace (t':tr)) ts') ts' ts'')


normalise :: [Rule] -> Term -> Term
normalise rules t = 
    case rewrite rules t of
        [] -> t
        (t':_) -> normalise rules t'
        
normaliseTrace :: [Rule] -> Term -> Trace
normaliseTrace rules t = Trace $ normaliseTrace' [t]
    where normaliseTrace' tr = 
            case rewrite rules (head tr) of
                [] -> tr
                (t':_) -> normaliseTrace' (t':tr)
    

