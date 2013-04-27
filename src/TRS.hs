module TRS where

import qualified Data.HashMap as M
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec.Expr (Assoc(AssocLeft,AssocRight))

import Terms

type Signature = M.Map String Integer
data Operation = FunOp String Integer | PrefixOp String Integer | 
         PostfixOp String Integer | InfixOp String Assoc Integer
         
data TRS = TRS [Operation] [Rule]

constOp s = FunOp s 0

instance Show Rule where
  show (Rule l r) = show l ++ " → " ++ show r
  
instance Eq Operation where
  FunOp s1 a1 == FunOp s2 a2 = (s1 == s2) && (a1 == a2)
  PrefixOp s1 _ == PrefixOp s2 _ = (s1 == s2)
  InfixOp s1 _ _ == InfixOp s2 _ _ = (s1 == s2)
  PostfixOp s1 _ == PostfixOp s2 _ = (s1 == s2)
  FunOp s1 a1 == PrefixOp s2 _ = (s1 == s2) && (a1 == 1)
  FunOp s1 a1 == InfixOp s2 _ _ = (s1 == s2) && (a1 == 2)
  FunOp s1 a1 == PostfixOp s2 _ = (s1 == s2) && (a1 == 1)
  PrefixOp s1 _ == FunOp s2 a2 = (s1 == s2) && (a2 == 1)
  InfixOp s1 _ _ == FunOp s2 a2 = (s1 == s2) && (a2 == 2)
  PostfixOp s1 _ == FunOp s2 a2 = (s1 == s2) && (a2 == 1)
  PrefixOp s1 _ == PostfixOp s2 _ = (s1 == s2)
  PostfixOp s1 _ == PrefixOp s2 _ = (s1 == s2)
  _ == _ = False

instance Show Operation where
  show (FunOp s 0) = "const " ++ s
  show (FunOp s a) = "function " ++ s ++ " " ++ show a
  show (PrefixOp s p) = "prefix " ++ s ++ " " ++ show p
  show (PostfixOp s p) = "postfix " ++ s ++ " " ++ show p
  show (InfixOp s AssocLeft p) = "infixl " ++ s ++ " " ++ show p
  show (InfixOp s AssocRight p) = "infixr " ++ s ++ " " ++ show p


isWellformed :: Signature -> Term -> Bool
isWellformed a (Var _) = True
isWellformed a (Fun f args) = M.lookup f a == Just (genericLength args) &&
                                  all (isWellformed a) args
  
opName :: Operation -> String
opName (FunOp s _) = s
opName (PrefixOp s _) = s
opName (InfixOp s _ _) = s
opName (PostfixOp s _) = s

opArity :: Operation -> Integer
opArity (FunOp _ a) = a
opArity (InfixOp _ _ _) = 2
opArity _ = 1

opPrio :: Operation -> Integer
opPrio (PrefixOp _ p) = p
opPrio (InfixOp _ _ p) = p
opPrio (PostfixOp _ p) = p

opIsOperator :: Operation -> Bool
opIsOperator (FunOp _ _) = False
opIsOperator _ = True

buildOpMap ops = foldl f M.empty ops
    where f m op = M.insert (opName op, opArity op) op m

prettyPrint ops t = prettyPrint' t (buildOpMap ops)
    where prettyPrint' (Var x) _ = x
          prettyPrint' (Fun f []) _ = f
          prettyPrint' (Fun f args) m =
              case (M.lookup (f, genericLength args) m) of
                  Just (PrefixOp _ p) -> f ++ escapeArg p (head args) m
                  Just (PostfixOp _ p) -> escapeArg p (head args) m ++ f
                  Just (InfixOp _ AssocLeft p) -> escapeArg (p-1) (head args) m ++ 
                      " " ++ f ++ " " ++ escapeArg p (last args) m
                  Just (InfixOp _ AssocRight p) -> escapeArg p (head args) m ++ 
                      " " ++ f ++ " " ++ escapeArg (p-1) (last args) m
                  _ -> f ++ "(" ++ intercalate "," (map (\x -> prettyPrint' x m) args) ++ ")"
          escapeArg _ (Var x) m = x
          escapeArg p t@(Fun f args) m =
            case (M.lookup (f, genericLength args) m) of
              Just (FunOp _ _) -> prettyPrint' t m
              Just op -> if opPrio op < p then 
                  "(" ++ prettyPrint' t m ++ ")" else prettyPrint' t m
              _ -> prettyPrint' t m

prettyPrintRule ops (Rule l r) = prettyPrint ops l ++ " → " ++ prettyPrint ops r

prettyPrintTrace ops (Trace tr) = intercalate "\n  → " (map (prettyPrint ops) (reverse tr))

prettyPrintSubst ops σ = intercalate "\n" $ 
    map (\(x,t) -> x ++ " ↦ " ++ prettyPrint ops t) $ M.toList σ


