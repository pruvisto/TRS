import Control.Monad
import System.IO
import System.IO.Error
import Data.List
import Data.Char
import System.Environment  
import Rewriting
import qualified Data.HashMap as M
import qualified TRS
import qualified Terms

extractTerm trs s = parseTerm trs $ tail $ dropWhile (/=' ') s

extractTerms trs s sep =
    if isInfixOf sep' s' then 
        Just (parseTerm trs s1, parseTerm trs s2)
    else
        Nothing
    where sep' = ' ' : sep ++ " "
          s' = tail (dropWhile (/=' ') s)
          (s1,s2) = split s' []
          split (c:s) acc
              | isPrefixOf sep' (c:s) = (acc, drop (length sep') (c:s))
              | otherwise = split s (acc ++ [c])
              
helpMessage = "Available commands:" ++ concatMap ("\n  "++) commands ++ "\n"
    
commands = ["rules", "unify", "match", "rewrite", "trace", "normalise", 
            "normalise_trace", "critical_pairs", "locally_confluent", "quit"]
commandHelpMessages = [
    "rules\n"++
    "    Prints a list of all the rules in the term rewriting system.\n",
    "unify <term1> and <term2>\n" ++
    "    Prints the most general unifier σ for the given two terms t1 and t2, " ++
    "    if a unifier exists. The term σ(t1) = σ(t2) is also printed.",
    "match <term1> to <term2>\n" ++
    "    Prints the matcher σ for the first given term t1 to the second one t2, " ++
    "    if it exists. A matcher is a substitution such that σ(t1) = t2.",
    "rewrite [<term>]\n"++
    "    Applies a single rewrite step to the given term and displays a list of\n"++
    "    all possible results. (There may be several, as several rules may be\n"++
    "    applicable, and the same rule may be applicable in different positions\n"++
    "    of the term.)\n"++
    "    If no term is given, the first term in the last result list is used.\n",
    "trace [<term>]\n"++
    "    Iteratively applies the rewrite rules until a normal form is reached and\n"++
    "    then displays a rewriting trace for every distinct normal form.\n"++
    "    If no term is given, the first term in the last result list is used.\n",
    "normalise [<term>]\n"++
    "    Eagerly applies all rewrite rules iteratively until a normal form is used\n"++
    "    and then returns this normal form.\n"++
    "    If no term is given, the first term in the last result list is used.\n",
    "normalise_trace [<term>]\n"++
    "    Eagerly applies all rewrite rules iteratively until a normal form is used\n"++
    "    and then returns a trace from the original term to this normal form.\n"++
    "    If no term is given, the first term in the last result list is used.\n",
    "critical_pairs\n"++
    "    Prints a list of all critical pairs of the term rewriting system.\n",
    "locally_confluent [verbose]\n"++
    "    Tries to determine whether the term rewriting system is locally \n"++
    "    confluent, i.e. whether any split of the form t1 ← s → t2 can be\n" ++
    "    rejoined with t1 →* t ←* t2.\n" ++
    "    If the option \"verbose\" is given, a list of all critical pairs and \n" ++
    "    whether they are joinable or not is also printed.\n",
    "quit\n"++
    "    Ends the programme.\n"]
    
doHelp s = case elemIndex s commands of
               Nothing -> putStrLn "No such command."
               Just i -> putStrLn ('\n' : commandHelpMessages !! i)

doShowRules trs = case trs of
    TRS.TRS _ rules -> putStrLn $ intercalate "\n" $ map (prettyPrintRule trs) rules

doUnify trs Nothing = putStrLn ("Invalid parameters. Please type " ++
                          "\"unify <term1> and <term2>\".") >> return Nothing
doUnify trs (Just (t1,t2)) =
    case unify (t1,t2) of
        Nothing -> putStrLn "No unifiers." >> return Nothing
        Just σ -> let t' = Terms.applySubst σ t1 
                   in putStrLn ("Most general unifier:\n" ++ 
                                 prettyPrintSubst trs σ ++ "\n\n" ++
                                 "Corresponding term: " ++ prettyPrint trs t') >>
                      return (Just t')
                      
doMatch trs Nothing = putStrLn ("Invalid parameters. Please type " ++
                          "\"match <term1> to <term2>\".")
doMatch trs (Just (t1,t2)) =
    case match (t1,t2) of
        Nothing -> putStrLn "No matchers."
        Just σ -> putStrLn ("Matcher:\n" ++ prettyPrintSubst trs σ)

doRewrite trs t = case ts of
    [] -> putStrLn "Term is already in normal form." >> return Nothing
    (t:_) -> do putStrLn $ intercalate "\n" $ 
                    zipWith (\a b -> show a ++ ": " ++ b) 
                    [1..genericLength ts] (map (prettyPrint trs) ts)
                return (Just t)
    where ts = rewrite trs t
    
doTrace trs t = case traces of
    [] -> putStrLn "No result." >> return Nothing
    _ -> do putStrLn $ intercalate "\n\n" $ 
                zipWith (\a b -> "Trace " ++ show a ++ ":\n" ++ b) 
                [1..genericLength traces] (map (prettyPrintTrace trs) traces)
            return Nothing
    where traces = rewriteTrace trs t
    
doNormalise trs t = putStrLn (prettyPrint trs (normalise trs t)) >> return Nothing

doNormaliseTrace trs t = putStrLn (prettyPrintTrace trs (normaliseTrace trs t)) >> return Nothing

doCriticalPairs trs = 
    case cps of 
        [] -> putStrLn "No critical pairs."
        _ -> putStr $ concatMap formatPair $ zip [1..genericLength cps] cps
    where cps = criticalPairs trs
          formatPair (i,(s,t1,t2)) = "Critical pair " ++ show i ++ ":\n" ++
                                     pretty s ++ " → " ++ pretty t1 ++ "\n" ++
                                     pretty s ++ " → " ++ pretty t2 ++ "\n\n"
          pretty t = prettyPrint trs t
          
doLocallyConfluent trs verbose = putStr $ 
        (if verbose then concatMap formatPair $ zip [1..ncps] cps else []) ++
        ("System is " ++ (if isWeaklyConfluent trs then [] else "not ") ++
              "locally confluent.\n")
    where cps = criticalPairs trs
          ncps = genericLength cps
          formatPair (i,(s,t1,t2)) = 
              let n1 = normalise trs t1
                  n2 = normalise trs t2
              in "Critical pair " ++ show i ++ ":\n" ++
                 pretty s ++ " → " ++ pretty t1 ++ " →* " ++ pretty n1 ++ "\n" ++
                 pretty s ++ " → " ++ pretty t2 ++ " →* " ++ pretty n2 ++  "\n" ++
                 if n1 == n2 then "Joinable\n\n" else "Not joinable\n\n"
          pretty t = prettyPrint trs t

commandLoop trs last = do
    putStr "TRS> "
    hFlush stdout
    str <- getLine
    case words (map toLower str) of
        [] -> commandLoop trs last
        "quit":_ -> return ()
        "help":[] -> putStrLn helpMessage >> commandLoop trs last
        "help":cmd:_ -> doHelp cmd >> commandLoop trs last
        "rules":_ -> doShowRules trs >> commandLoop trs last
        "rewrite":[] -> case last of
            Nothing -> putStrLn "No current result. Type 'rewrite <term>'" >> commandLoop trs Nothing
            Just t -> doRewrite trs t  >>= commandLoop trs
        "trace":[] -> case last of
            Nothing -> putStrLn "No current result. Type 'trace <term>'" >> commandLoop trs Nothing
            Just t -> doTrace trs t >>= commandLoop trs
        "normalise":[] -> case last of
            Nothing -> putStrLn "No current result. Type 'normalise <term>'" >> commandLoop trs Nothing
            Just t -> doNormalise trs t  >>= commandLoop trs
        "normalise_trace":[] -> case last of
            Nothing -> putStrLn "No current result. Type 'normalise_trace <term>'" >> commandLoop trs Nothing
            Just t -> doNormaliseTrace trs t  >>= commandLoop trs
        "unify":_ -> doUnify trs (extractTerms trs str "and") >>= commandLoop trs
        "match":_ -> doMatch trs (extractTerms trs str "to") >> commandLoop trs last
        "rewrite":_ -> doRewrite trs (extractTerm trs str) >>= commandLoop trs
        "trace":_ -> doTrace trs (extractTerm trs str) >>= commandLoop trs
        "normalise":_ -> doNormalise trs (extractTerm trs str) >>= commandLoop trs
        "normalise_trace":_ -> doNormaliseTrace trs (extractTerm trs str) >>= commandLoop trs
        "critical_pairs":_ -> doCriticalPairs trs >> commandLoop trs Nothing
        "locally_confluent":"verbose":_-> doLocallyConfluent trs True >> commandLoop trs Nothing
        "locally_confluent":_ -> doLocallyConfluent trs False >> commandLoop trs Nothing
        xs:_ -> putStrLn ("Unknown command: " ++ xs) >> commandLoop trs last

main = do
    args <- getArgs
    if genericLength args < 1 then
        putStrLn "Usage: rewrite <TRS file>"
    else do
        trs <- parseTRSFile (head args)
        catch (commandLoop trs Nothing) (\e -> if isEOFError e then putStrLn "" else ioError e)



