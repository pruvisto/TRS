module Confluence where

import TRS
import Terms
import Data.Maybe
import Control.Applicative
import qualified Data.HashSet as S
import qualified Data.HashMap as M

makeVarRenaming :: S.Set String -> [String] -> M.Map String String
makeVarRenaming v1 v2 = foldl renameVar M.empty v2
    where renameVar m x 
              | S.member x v1 = M.insert x (findFreeName x 2) m
              | otherwise = m
          findFreeName x n
              | S.member (x ++ show n) v1 = findFreeName x (n+1)
              | otherwise = x ++ show n
              
makeVarsDisjoint :: (Rule,Rule) -> (Rule, Rule)
makeVarsDisjoint (Rule l1 r1, Rule l2 r2) = 
        (Rule l1 r1, renameRuleVars' (Rule l2 r2))
    where renameRuleVars' = renameRuleVars (\x -> maybe x id (M.lookup x m))
          m = makeVarRenaming v1 (S.toList v2)
          v1 = S.union (variablesHashSet l1) (variablesHashSet r1)
          v2 = S.union (variablesHashSet l2) (variablesHashSet r2)
          renameRuleVars f (Rule l r) = Rule (renameVars f l) (renameVars f r)
    
criticalPairs' :: Term -> Term -> Term -> Term -> Bool -> [(Term,Term,Term)]
criticalPairs' (Var _) _ _ _ _ = []
criticalPairs' _ _ (Var _) _ _ = []
criticalPairs' s@(Fun f args) r1 l2 r2 noRoot = 
        (if noRoot then [] else maybeToList critPair) ++ childrenCrits [] args
    where critPair = (\σ -> (applySubst σ s, applySubst σ r1, applySubst σ r2)) 
                         <$> unifyMaybe (s,l2)
          childrenCrits leftArgs [] = []
          childrenCrits leftArgs (x:rightArgs) = 
              [(Fun f (leftArgs ++ [s] ++ rightArgs), t1, 
                Fun f (leftArgs ++ [t2] ++ rightArgs))
                  | (s,t1,t2) <- criticalPairs' x r1 l2 r2 False] ++
              childrenCrits (leftArgs ++ [x]) rightArgs
              
criticalPairs :: [Rule] -> [(Term,Term,Term)]
criticalPairs [] = []
criticalPairs (r:rs) = criticalPairs'' r r True ++ 
                       concatMap critPairsWithR rs ++ 
                       criticalPairs rs
    where critPairsWithR r' = criticalPairs'' r r' False ++ criticalPairs'' r' r False
          criticalPairs'' r1 r2 b = 
            case makeVarsDisjoint (r1,r2) of
              (Rule l1 r1, Rule l2 r2) -> criticalPairs' l1 r1 l2 r2 b
                       
isWeaklyConfluent :: [Rule] -> Bool
isWeaklyConfluent rules = all equivalent (criticalPairs rules)
    where equivalent (_,s,t) = normalise rules s == normalise rules t


