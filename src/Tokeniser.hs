module Tokeniser where

import Data.Char
import Data.List
import Control.Monad.Identity
import Control.Applicative ((<$>), (<*>))
import qualified Text.Parsec.Prim
import Text.ParserCombinators.Parsec


data Token = LPR | RPR | Quot | Comma | Reserved String | FunName String | OpName String |
             VarName String deriving Eq


instance Show Token where
    show LPR = "'('"
    show RPR = "')'"
    show Quot = "'\"'"
    show Comma = ","
    show (Reserved s) = "keyword '" ++ s ++ "'"
    show (FunName s) = "identifier '" ++ s ++ "'"
    show (OpName s) = "operation '" ++ s ++ "'"
    show (VarName s) = "variable '" ++ s ++ "'"

trsValidOperatorChar c = c `notElem` "(),\"→" && not (isAlphaNum c) && not (isSpace c) && isPrint c
varName = (:) <$> satisfy isUpper <*> many (satisfy isAlphaNum)
funName = (:) <$> satisfy (\c -> isLower c || isDigit c) <*> many (satisfy isAlphaNum)
opName = many1 (satisfy trsValidOperatorChar)

trsReservedNames = ["section", "signature", "rules", "rule", 
                    "function", "prefix", "postfix",
                    "infixl", "infixr", "const", "->", "→"]

tkn   = (char '(' >> return LPR) <|>
        (char ')' >> return RPR) <|>
        (char '"' >> return Quot) <|>
        (char ',' >> return Comma) <|>
        (choice (map (try . string) trsReservedNames) >>= return . Reserved) <|>
        (varName >>= return . VarName) <|>
        (funName  >>= return . FunName) <|>
        (opName >>= return . OpName)
        
tokens = do {spaces; tks <- many (do {spaces; t <- tkn; spaces; return t}); spaces; return tks }

stripComments = unlines . map stripComment . lines
    where stripComment xs = if isInfixOf "--" xs then stripComment' xs else xs
          stripComment' ('-':'-':_) = []
          stripComment' (x:xs) = x : stripComment' xs

tokenise :: String -> [Token]
tokenise s = either (\s -> error (show s)) id (parse parser "" s')
    where parser = do { x <- Tokeniser.tokens; eof; return x }
          s' = stripComments s




