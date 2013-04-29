module Rewriting where

import Control.Monad
import Control.Applicative
import qualified TRS
import qualified Terms
import qualified Parser
import qualified Confluence

parseTRS = Parser.parseTRS
parseTRSFile f = parseTRS <$> readFile f
parseTerm (TRS.TRS ops _) = Parser.parseTerm ops
parseTermEither (TRS.TRS ops _) = Parser.parseTermEither ops
parseRule (TRS.TRS ops _) = Parser.parseRule ops
match (s,t) = Terms.match (s,t)
unify (s,t) = Terms.unify (s,t)
rewrite (TRS.TRS _ rules) term = Terms.rewrite rules term
rewriteStar (TRS.TRS _ rules) term = Terms.rewriteStar rules term
rewriteTrace (TRS.TRS _ rules) term = Terms.rewriteTrace rules term
normalise (TRS.TRS _ rules) term = Terms.normalise rules term
normaliseTrace (TRS.TRS _ rules) term = Terms.normaliseTrace rules term
prettyPrint (TRS.TRS ops _) term = TRS.prettyPrint ops term
prettyPrintRule (TRS.TRS ops _) rule = TRS.prettyPrintRule ops rule
prettyPrintSubst (TRS.TRS ops _) σ = TRS.prettyPrintSubst ops σ
prettyPrintTrace (TRS.TRS ops _) tr = TRS.prettyPrintTrace ops tr
criticalPairs (TRS.TRS _ rules) = Confluence.criticalPairs rules
isWeaklyConfluent (TRS.TRS _ rules) = Confluence.isWeaklyConfluent rules


