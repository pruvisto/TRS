module Parser where

import Data.Char
import Data.List
import Data.Function
import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>))
import qualified Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos
import qualified Data.Foldable as F
import qualified Data.HashMap as M

import qualified TRS
import Terms
import Tokeniser


oneTokenP f = token show (\_ -> initialPos "Dummy") f
oneToken t = token show (\_ -> initialPos (show t)) 
                  (\t' -> if t == t' then Just () else Nothing)
opNameToken = oneTokenP (\t -> case t of {OpName s -> Just s; _ -> Nothing})
funNameToken = oneTokenP (\t -> case t of {FunName s -> Just s; _ -> Nothing})
reserved s = oneToken (Reserved s)
parenLeft = oneToken LPR
parenRight = oneToken RPR
quotMark = oneToken Quot
comma = oneToken Comma

operationsToSignature :: [TRS.Operation] -> Maybe TRS.Signature
operationsToSignature ops = foldl f (Just M.empty) ops
    where f :: Maybe TRS.Signature -> TRS.Operation -> Maybe TRS.Signature
          f a (TRS.FunOp s arity) = ins s arity <$> a
          f a (TRS.PrefixOp s _) = ins s 1 <$> a
          f a (TRS.InfixOp s _ _) = ins s 2 <$> a
          f a (TRS.PostfixOp s _) = ins s 1 <$> a
          ins k v m = M.insertWith union k [v] m
          
opToParser :: TRS.Operation -> Operator Token st Term
opToParser (TRS.InfixOp s assoc p) = Infix (oneToken (OpName s) >> return (\a b -> Fun s [a,b])) assoc
opToParser (TRS.PrefixOp s p) = Prefix (oneToken (OpName s) >> return (\a -> Fun s [a]))
opToParser (TRS.PostfixOp s p) = Postfix (oneToken (OpName s) >> return (\a -> Fun s [a]))

operationsToOpTable :: [TRS.Operation] -> [[Operator Token st Term]]
operationsToOpTable ops = map (map opToParser) $ groupBy ((==) `on` TRS.opPrio) $ 
                              sortBy (compare `on` (negate.TRS.opPrio)) $ filter TRS.opIsOperator ops

fun :: [[Operator Token st Term]] -> TRS.Signature -> Text.Parsec.Prim.ParsecT [Token] st Identity Term
fun ops sig =
    do
        f <- funNameToken <|> opNameToken
        args <- optionMaybe (between parenLeft parenRight (sepBy (term ops sig) comma))
        case M.lookup f sig of 
            Nothing -> fail ("No such function: " ++ f)
            Just arities -> do
                let arity = maybe 0 genericLength args
                if arity `notElem` arities
                    then fail ("Wrong number of parameters for function " ++ f ++
                                "\nExpected: " ++ intercalate ", " (map show arities) ++  "\n" ++
                                " Actual: " ++ show arity)
                    else return (Fun f (F.concat args))

var = oneTokenP (\t -> case t of {VarName s -> Just (Var s); _ -> Nothing})

term ops sig = buildExpressionParser ops $
                   between parenLeft parenRight (term ops sig) <|> fun ops sig <|> var

rule ops sig = do
    l <- term ops sig
    reserved "→" <|> reserved "->"
    r <- term ops sig
    return (Rule l r)
               
               
constOpDecl = reserved "const" >> quotMark >> funNameToken >>= 
                  (\f -> quotMark >> return (TRS.FunOp f 0))

funOpDecl = reserved "function" >> quotMark >> funNameToken >>=
                (\f -> quotMark >> funNameToken >>= (\n ->
                return (TRS.FunOp f (read n))))

fixOpDecl s f = reserved s >> quotMark >> opNameToken >>= (\op ->
                    quotMark >> funNameToken >>= (\n -> 
                    return (f op (read n))))
        
prefixOpDecl = fixOpDecl "prefix" TRS.PrefixOp
postfixOpDecl = fixOpDecl "postfix" TRS.PostfixOp
infixlOpDecl = fixOpDecl "infixl" (\op a -> TRS.InfixOp op AssocLeft a)    
infixrOpDecl = fixOpDecl "infixr" (\op a -> TRS.InfixOp op AssocRight a)
opDecl = constOpDecl <|> funOpDecl <|> prefixOpDecl <|> 
               postfixOpDecl <|> infixlOpDecl <|> infixrOpDecl

sigParser =
    do reserved "section"
       reserved "signature"
       ops <- many opDecl
       return ops

ruleDecl ops sig = reserved "rule" >> quotMark >> rule ops sig >>= (\r ->
                       quotMark >> return r)

rulesParser ops sig = reserved "section" >> reserved "rules" >> 
                          many (ruleDecl ops sig)
       
checkOpsConsistency [] = return ()
checkOpsConsistency (x:xs)
    | all (/= x) xs = checkOpsConsistency xs
    | otherwise = fail ("Multiple operation declarations for operation " ++
                            TRS.opName x ++ ":\n" ++ ops)
          where ops = intercalate "\n" $ map show $ filter (==x) (x:xs)

trsParser =
    do ops <- sigParser
       checkOpsConsistency ops
       let sig = case operationsToSignature ops of Just s -> s
       let opTable = operationsToOpTable ops
       rules <- rulesParser opTable sig
       return (TRS.TRS ops rules)
       
parseTRS s = either (\s -> error (show s)) id (parse parser "" s')
    where parser = do {e <- trsParser; eof; return e}
          s' = tokenise s
          
parseTerm ops s = either (\s -> error (show s)) id (parse parser "" s')
    where optable = operationsToOpTable ops
          sig = case operationsToSignature ops of Just a -> a
          parser = do {e <- term optable sig; eof; return e}
          s' = tokenise s
          
parseTermEither ops s = parse parser "" s'
    where optable = operationsToOpTable ops
          sig = case operationsToSignature ops of Just a -> a
          parser = do {e <- term optable sig; eof; return e}
          s' = tokenise s

parseRule ops s = either (\s -> error (show s)) id (parse parser "" s')
    where optable = operationsToOpTable ops
          sig = case operationsToSignature ops of Just a -> a
          parser = do {e <- rule optable sig; eof; return e}
          s' = tokenise s

