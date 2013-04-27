module TRSParser where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.HashMap as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.String

import TRS
import Terms

{- Signature parser -}

constParser = do
    spaces
    string "const"
    skipMany1 space
    f <- funNameParser
    return (constOp f)
    
funOpParser = do
    spaces
    string "function"
    skipMany1 space
    f <- funNameParser
    skipMany1 space
    a <- many1 digit
    return (FunOp f (read a))

fixOpParser s f = do
    spaces
    string s
    skipMany1 space
    op <- opNameParser
    skipMany1 space
    a <- many1 digit
    return (f op (read a))
    
prefixOpParser = fixOpParser "prefix" PrefixOp
postfixOpParser = fixOpParser "postfix" PostfixOp
infixlOpParser = fixOpParser "infixl" (\op a -> InfixOp op AssocLeft a)    
infixrOpParser = fixOpParser "infixr" (\op a -> InfixOp op AssocRight a)
opParser = try constParser <|> funOpParser <|> try prefixOpParser <|> 
               try postfixOpParser <|> 
               try infixlOpParser <|> infixrOpParser
     
space' = satisfy (\c -> isSpace c && c /= '\n')
spaces' = many space'
eol = spaces' >> char '\n' >> spaces
               
sigParser = do
    spaces'
    string "section"
    skipMany1 space'
    string "signature"
    eol
    ops <- many ((\x _ -> x) <$> opParser <*> eol)
    return ops
    
rulesParser ops sig = do
    spaces'
    string "section"
    skipMany1 space'
    string "rules"
    eol
    rules <- many ((\x _ -> x) <$> ruleParser ops sig <*> eol)
    return rules
    
checkOpsConsistency [] = return ()
checkOpsConsistency (x:xs)
    | all (/= x) xs = checkOpsConsistency xs
    | otherwise = fail ("Multiple operation declarations for operation " ++
                            opName x ++ ":\n" ++ ops)
          where ops = intercalate "\n" $ map show $ filter (==x) (x:xs)
    
trsParser = do
    ops <- sigParser
    checkOpsConsistency ops
    let sig = case operationsToSignature ops of Just s -> s
    rules <- rulesParser (operationsToOpTable ops) sig
    eof
    return (TRS ops rules)
    
parseTRS s = either (\s -> error (show s)) id (parse trsParser "" (stripComments s))
    where stripComments = unlines . map stripComment . lines
          stripComment xs = if isInfixOf "--" xs then stripComment' xs else xs
          stripComment' ('-':'-':_) = []
          stripComment' (x:xs) = x : stripComment' xs
    

{- Term parser -}

operationsToSignature :: [Operation] -> Maybe Signature
operationsToSignature ops = foldl f (Just M.empty) ops
    where f a (FunOp s arity) = a >>= (ins s arity)
          f a (PrefixOp s _) = a >>= (ins s 1)
          f a (InfixOp s _ _) = a >>= (ins s 2)
          f a (PostfixOp s _) = a >>= (ins s 1)
          ins k v m = maybe (Just (M.insert k v m)) (const Nothing) (M.lookup k m)
          

opToParser (InfixOp s assoc p) = Infix (spaces' >> string s >> spaces' >> return (\a b -> Fun s [a,b])) assoc
opToParser (PrefixOp s p) = Prefix (spaces' >> string s >> spaces' >> return (\a -> Fun s [a]))
opToParser (PostfixOp s p) = Postfix (spaces' >> string s >> spaces' >> return (\a -> Fun s [a]))

operationsToOpTable :: [Operation] -> [[Operator Char st Term]]
operationsToOpTable ops = map (map opToParser) $ groupBy ((==) `on` opPrio) $ 
                              sortBy (compare `on` (negate.opPrio)) $ filter opIsOperator ops

validOperatorChar c = c /= '(' && c /= ')' && c /= '→' && 
                          not (isAlphaNum c) && not (isSpace c) && isPrint c
  
funNameParser = ((:) <$> satisfy (\c -> isLower c || isDigit c) <*> many alphaNum)
opNameParser = many1 (satisfy (validOperatorChar))
  
funParser ops sig = do
    spaces'
    f <- funNameParser <|> opNameParser
    guard (f /= "->")
    spaces'
    args <- optionMaybe (between (char '(') (char ')') (sepBy (exprParser ops sig) (char ',')))
    spaces'
    case M.lookup f sig of 
        Nothing -> fail ("No such function: " ++ f)
        Just arity -> do
            let arity' = maybe 0 genericLength args
            if arity /= arity'
                then fail ("Wrong number of parameters for function " ++ f ++
                            "\nExpected: " ++ show arity ++ " Actual: " ++ show arity')
                else return (Fun f (F.concat args))

varParser = do
    spaces'
    x <- (:) <$> upper <*> many alphaNum 
    spaces'
    return (Var x)
    
parParser ops sig = do
    spaces'
    x <- (between (char '(') (char ')') (exprParser ops sig)) 
    spaces'
    return x
        
termParser ops sig = do
    spaces'
    x <- try (parParser ops sig) <|> 
         try (funParser ops sig) <|>
         varParser
    spaces'
    return x

exprParser ops sig = do
    spaces'
    x <- buildExpressionParser ops (termParser ops sig)
    spaces'
    return x

ruleParser ops sig = do
    l <- exprParser ops sig
    spaces
    try (string "->") <|> string "→"
    spaces
    r <- exprParser ops sig
    return (Rule l r)
    
parseTerm ops s = either (\s -> error (show s)) id (parse parser "" s)
    where optable = operationsToOpTable ops
          sig = case operationsToSignature ops of Just a -> a
          parser = do
                       e <- exprParser optable sig
                       eof
                       return e

parseRule ops s = either (\s -> error (show s)) id (parse parser "" s)
    where optable = operationsToOpTable ops
          sig = case operationsToSignature ops of Just a -> a
          parser = do
                       e <- ruleParser optable sig
                       eof
                       return e

