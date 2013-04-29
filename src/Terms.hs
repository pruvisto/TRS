module Terms where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative ((<$>))
import qualified Data.HashMap as M
import qualified Data.Set as S
import qualified Data.HashSet as HS

data Term = Var String | Fun String [Term] deriving (Eq, Ord)
data Rule = Rule Term Term deriving (Eq, Ord)
newtype Trace = Trace [Term]
type Subst = M.Map String Term

instance Show Term where
  show (Var x) = x
  show (Fun f []) = f
  show (Fun f args) = f ++ "(" ++ intercalate "," (map show args) ++ ")"
  
instance Show Trace where
  show (Trace tr) = intercalate "\n  → " (map show (reverse tr))
  
instance Eq Trace where
  Trace [] == Trace [] = True
  Trace [] == _ = False
  _ == Trace [] = False
  Trace l1 == Trace l2 = head l1 == head l2
  
instance Ord Trace where
  compare (Trace []) (Trace []) = EQ
  compare (Trace []) _ = LT
  compare _ (Trace []) = GT
  compare (Trace l1) (Trace l2) = compare (head l1) (head l2)


variablesHashSet :: Term -> HS.Set String
variablesHashSet t = variablesHashSet' t HS.empty
    where variablesHashSet' (Var x) s = HS.insert x s
          variablesHashSet' (Fun _ args) s = 
              foldl (\s x -> variablesHashSet' x s) s args

variables :: Term -> [String]
variables = HS.toList . variablesHashSet

containsVar :: String -> Term -> Bool
containsVar x (Var y) = x == y
containsVar x (Fun _ args) = any (containsVar x) args

substVar :: String -> Term -> Term -> Term
substVar x t (Var y) = if x == y then t else Var y
substVar x t (Fun f args) = Fun f (map (substVar x t) args)

applySubst :: Subst -> Term -> Term
applySubst σ (Var x) = maybe (Var x) id (M.lookup x σ)
applySubst σ (Fun f fargs) = Fun f (map (applySubst σ) fargs)

updateSubst :: String -> Term -> Subst -> Subst
updateSubst x t σ = M.insert x t $ M.map subst σ
    where subst (Var y) = if x == y then t else Var y
          subst (Fun f args) = Fun f (map subst args)
          
renameVars :: (String -> String) -> Term -> Term
renameVars f (Var x) = Var (f x)
renameVars f (Fun g args) = Fun g (map (renameVars f) args)
          
          

isGround :: Term -> Bool
isGround (Var _) = False
isGround (Fun _ args) = all isGround args


data UnificationError = OccursCheck String Term | Clash Term Term deriving Eq

instance Show UnificationError where
    show (OccursCheck x t) = "occurs check failed at " ++ x ++ " =!= " ++ show t
    show (Clash s t) = "clash at " ++ show s ++ " =!= " ++ show t

match :: (Term, Term) -> Either UnificationError Subst
match p = match' p M.empty
    where match' (s@(Fun f fArgs), t@(Fun g gArgs)) σ
            | f /= g || genericLength fArgs /= genericLength gArgs = Left (Clash  s t)
            | otherwise = foldl (\s t1t2 -> either Left (match' t1t2) s) (Right σ) (zip fArgs gArgs)
          match' (Var x, t) σ =
               case M.lookup x σ of
                 Nothing -> Right (M.insert x t σ)
                 Just t' -> if t == t' then Right σ else Left (Clash t t')
          match' (s, t@(Var _)) _ = Left (Clash s t)
                 
matchMaybe :: (Term, Term) -> Maybe Subst
matchMaybe p = either (const Nothing) Just (match p)


-- Propagates the assignment of the variable x to the term t to the 
-- given constraint.
updateConstr x t (t1,t2) = (substVar x t t1, substVar x t t2)

-- Finds the most general substitution σ that makes the lhs and rhs of all 
-- constraints in the given list equal.
unify' :: [(Term,Term)] -> Subst -> Either UnificationError Subst
unify' [] σ = Right σ
unify' ((Var x, t@(Var y)) : constrs) σ
    | x == y = unify' constrs σ    -- trivial constraint
    | genericLength x < genericLength y = unify' ((Var y, Var x):constrs) σ
    | otherwise = unify' (map (updateConstr x t) constrs) (updateSubst x t σ)
unify' ((Var x, t) : constrs) σ
    | containsVar x t = Left (OccursCheck x t)
    | otherwise = unify' (map (updateConstr x t) constrs) (updateSubst x t σ)
unify' ((t, Var x):constrs) σ = unify' ((Var x,t):constrs) σ
unify' ((s@(Fun f fArgs), t@(Fun g gArgs)) : constrs) σ
    | f /= g || genericLength fArgs /= genericLength gArgs = Left (Clash s t)
    | otherwise = unify' (zip fArgs gArgs ++ constrs) σ

-- Finds the most general unifier σ for the given terms s and t, 
-- i.e. a substitution such that σ(s) = σ(t).
-- "Most general" means that for any other substituion σ' that also 
-- satisfies σ'(s) = σ'(t), there is a substitution τ s.t. τσ = σ'
-- On success, "Left σ" is returned, otherwise "Right e" with the 
-- error e that caused the unification to fail.
unify :: (Term, Term) -> Either UnificationError Subst
unify p = unify' [p] M.empty

-- Same as unify, but returns Maybe instead of Either
unifyMaybe :: (Term, Term) -> Maybe Subst
unifyMaybe p = either (const Nothing) Just (unify p)

-- Finds the most general unifier for all the terms in the given list
unifyAll :: [Term] -> Either UnificationError Subst
unifyAll ts = unifyAll' ts M.empty
    where unifyAll' (t1:t2:ts) σ =
              case unify' [(applySubst σ t1, applySubst σ t2)] σ of
                  Right σ -> unifyAll' (t1:ts) σ
                  Left e -> Left e
          unifyAll' _ σ = Right σ

-- Given a rule r and a list of terms [t1,…,tn] picks an i, rewrites ti → ti'
-- and returns [t1,…,ti',…,tn]
rewriteOneWith :: Rule -> [Term] -> [[Term]]
rewriteOneWith rule ts = snd (foldr f ([], []) ts)
    where f t (ts, acc) = (t:ts, map (:ts) (rewriteWith rule t) ++ map (t:) acc)

-- Performs a single rewrite step on the given term with the given rule
rewriteWith :: Rule -> Term -> [Term]
rewriteWith rule@(Rule l r) t = 
   maybeToList ((\σ -> applySubst σ r) <$> matchMaybe (l, t)) ++
   case t of
       Var _ -> []
       Fun f args -> map (Fun f) (rewriteOneWith rule args)

-- Performs a single rewrite step on the given term with any of the given rules
rewrite :: [Rule] -> Term -> [Term]
rewrite rules t = rules >>= (\rule -> rewriteWith rule t)

-- Returns all terms reachable from the given term by rewriting it with 
-- the given rules, i.e. enumerates the →* relation
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

-- Enumerates the →* relation and shows a trace of rewrite steps for every term
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

-- Eagerly applies rewrite steps until an irreducible term is obtained.
-- If there is an infinitely descending chain starting at the given term, 
-- this function may not terminate, even if there is a reachable irredicuble
-- term.
normalise :: [Rule] -> Term -> Term
normalise rules t = 
    case rewrite rules t of
        [] -> t
        (t':_) -> normalise rules t'

-- Same as normalise, but also returns a trace of the rewrite steps 
-- along the way
normaliseTrace :: [Rule] -> Term -> Trace
normaliseTrace rules t = Trace $ normaliseTrace' [t]
    where normaliseTrace' tr = 
            case rewrite rules (head tr) of
                [] -> tr
                (t':_) -> normaliseTrace' (t':tr)
    

