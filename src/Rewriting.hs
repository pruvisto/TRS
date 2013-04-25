module Rewriting where

import Control.Monad
import Control.Applicative
import qualified TRS
import qualified Terms
import qualified TRSParser

parseTRS = TRSParser.parseTRS
parseTRSFile f = parseTRS <$> readFile f
parseTerm (TRS.TRS ops _) = TRSParser.parseTerm ops
parseRule = TRSParser.parseRule
rewrite (TRS.TRS _ rules) term = Terms.rewrite rules term
rewriteStar (TRS.TRS _ rules) term = Terms.rewriteStar rules term
rewriteTrace (TRS.TRS _ rules) term = Terms.rewriteTrace rules term
normalise (TRS.TRS _ rules) term = Terms.normalise rules term
normaliseTrace (TRS.TRS _ rules) term = Terms.normaliseTrace rules term
prettyPrint (TRS.TRS ops _) term = TRS.prettyPrint ops term
prettyPrintTrace (TRS.TRS ops _) tr = TRS.prettyPrintTrace ops tr


