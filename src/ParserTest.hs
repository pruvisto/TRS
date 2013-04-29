module Tokeniser where

import Data.Char
import Data.List
import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>))
import qualified Text.Parsec.Prim
import Text.ParserCombinators.Parsec

data Token = LPR | RPR | Reserved String | FunName String | OpName String |
             VarName String | EOL deriving (Show, Eq)

space' = satisfy (\c -> isSpace c && c /= '\n' && c /= '\r')
spaces' = many space'
             
trsValidOperatorChar c = c /= '(' && c /= ')' && c /= '→' && 
                          not (isAlphaNum c) && not (isSpace c) && isPrint c
varName = (:) <$> satisfy isUpper <*> many (satisfy isAlphaNum)
funName = (:) <$> satisfy (\c -> isLower c || isDigit c) <*> many (satisfy isAlphaNum)
opName = many1 (satisfy trsValidOperatorChar)

trsReservedNames = ["section", "signature", "rules", "prefix", "postfix",
                 "infixl", "infixr", "const", "->", "→"]

tkn   = (char '(' >> return LPR) <|>
        (char ')' >> return RPR) <|>
        (satisfy (\c -> c == '\n' || c == '\r') >> spaces >> return EOL) <|>
        (try (choice (map string trsReservedNames)) >>= return . Reserved) <|>
        (varName >>= return . VarName) <|>
        (funName  >>= return . FunName) <|>
        (opName >>= return . OpName)
        
tokens = (\x _ -> x) <$> many ((\_ x -> x) <$> spaces' <*> tkn) <*> spaces

stripComments = unlines . map stripComment . lines
stripComment xs = if isInfixOf "--" xs then stripComment' xs else xs
stripComment' ('-':'-':_) = []
stripComment' (x:xs) = x : stripComment' xs

tokenise :: String -> [Token]
tokenise s = either (\s -> error (show s)) id (parse parser "" s)
    where parser = do { x <- Tokeniser.tokens; eof; return x }




