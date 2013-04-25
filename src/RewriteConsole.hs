import Control.Monad
import System.IO
import System.IO.Error
import Data.List
import Data.Char
import System.Environment  
import Rewriting
import qualified TRS

extractTerm trs s = parseTerm trs $ tail $ dropWhile (/=' ') s

helpMessage = "Available commands:" ++ concatMap ("\n  "++) commands ++ "\n"
    
commands = ["rewrite", "trace", "normalise", "normalise_trace", "quit"]
commandHelpMessages = [
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
    "quit\n"++
    "    Ends the programme.\n"]
    
doHelp s = case elemIndex s commands of
               Nothing -> putStrLn "No such command."
               Just i -> putStrLn ('\n' : commandHelpMessages !! i)

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

commandLoop trs last = do
    putStr "TRS> "
    hFlush stdout
    str <- catch getLine (\e -> if isEOFError e then return "quit" else ioError e)
    case words (map toLower str) of
        [] -> commandLoop trs last
        "quit":_ -> return ()
        "help":[] -> putStrLn helpMessage >> commandLoop trs last
        "help":cmd:_ -> doHelp cmd >> commandLoop trs last
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
        "rewrite":_ -> doRewrite trs (extractTerm trs str) >>= commandLoop trs
        "trace":_ -> doTrace trs (extractTerm trs str) >>= commandLoop trs
        "normalise":_ -> doNormalise trs (extractTerm trs str) >>= commandLoop trs
        "normalise_trace":_ -> doNormaliseTrace trs (extractTerm trs str) >>= commandLoop trs
        xs:_ -> putStrLn ("Unknown command: " ++ xs) >> commandLoop trs last

main = do
    args <- getArgs
    if genericLength args < 1 then
        putStrLn "Usage: rewrite <TRS file>"
    else do
        trs <- parseTRSFile (head args)
        commandLoop trs Nothing

