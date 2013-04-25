module TRS where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec.Expr (Assoc(AssocLeft,AssocRight))

data Term = Var String | Fun String [Term] deriving (Eq, Ord)
data Rule = Rule Term Term deriving (Eq, Ord)
newtype Trace = Trace [Term]
type Subst = M.Map String Term
type Signature = M.Map String Integer
data Operation = FunOp String Integer | PrefixOp String Integer | 
         PostfixOp String Integer | InfixOp String Assoc Integer
         
data TRS = TRS [Operation] [Rule]

constOp s = FunOp s 0
         
instance Show Term where
  show (Var x) = x
  show (Fun f []) = f
  show (Fun f args) = f ++ "(" ++ intercalate "," (map show args) ++ ")"
  
instance Show Rule where
  show (Rule l r) = show l ++ " → " ++ show r
  
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


prettyPrintTrace ops (Trace tr) = intercalate "\n  → " (map (prettyPrint ops) (reverse tr))

