{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Reminder to self: On Windows CMD, do chcp 65001 first,
-- so we can have nice things.

import Control.Applicative
import Control.Monad (ap)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (catMaybes)

data Exp = Var String
         | App [Exp]
         | Lambda String Exp
         deriving Show

data PPContext = PPTop | PPApp | PPLambdaBody

enclose :: String -> String
enclose str = "(" ++ str ++ ")"

pp :: Exp -> String
pp e = loop PPTop e
  where
    loop _ (Var x) = x
    loop PPApp (App es) = enclose $ ppApp es
    loop _ (App es) = ppApp es
    loop PPApp (Lambda x e) = enclose $ ppLambda x e
    loop _ (Lambda x e) = ppLambda x e
    ppApp es = unwords $ map (loop PPApp) es
    ppLambda x e = "λ" ++ x ++ "." ++ (loop PPLambdaBody e)

data Token = TLambda | TLParen | TRParen | TDot | TAtom String
             deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = catMaybes . joinTokens . fmap s2t
  where
    s2t '\\' = Just TLambda
    s2t 'λ' = Just TLambda
    s2t '.' = Just TDot
    s2t '(' = Just TLParen
    s2t ')' = Just TRParen
    s2t ws | isSpace ws = Nothing
    s2t other = Just (TAtom [other])
    joinTokens = foldr combine []
    combine (Just (TAtom l)) ((Just (TAtom r)): t) =
      (Just $ TAtom $ l ++ r) : t
    combine other lst = other : lst

data ParseResult i a = Parsed a i
                     | ParseError String
                     deriving (Functor, Show)

newtype Parser i a = Parser(i -> ParseResult i a)
                     deriving Functor

type TokenParser = Parser [Token]

runParser :: Parser i a -> i -> ParseResult i a
runParser (Parser f) s = f s

instance Monad (Parser i) where
  Parser f >>= g =
    Parser(\s -> case (f s) of
      ParseError error  -> ParseError error
      Parsed a s -> runParser (g a) s)
  return a = Parser(\s -> Parsed a s)

instance Applicative (Parser i) where
  pure a = Parser(\s -> Parsed a s)
  (<*>) = ap

parseAtom :: TokenParser String
parseAtom = Parser(impl)
  where
    impl (TAtom a:t) = Parsed a t
    impl _ = ParseError "Not an atom."

parseVar :: TokenParser Exp
parseVar = do
  name <- parseAtom
  return (Var name)

peek :: (Token -> Bool) -> TokenParser Bool
peek pred = Parser(impl) where
  impl [] = Parsed False []
  impl lst@(h:t) = Parsed (pred h) lst

condMOrElse :: Monad m => [(m Bool, m a)] -> m a -> m a
condMOrElse [] fallback = fallback
condMOrElse ((mc, mv) : t) fallback = do
  c <- mc
  if c then mv
       else condMOrElse t fallback

consume :: (Eq a, Show a) => a -> Parser [a] ()
consume expected = Parser(impl)
  where
    impl (h : t) | h == expected = Parsed () t
    impl (wrong : _) =
      ParseError ("Expected " ++ (show expected) ++
                  " but found " ++ (show wrong))
    impl [] =
      ParseError ("Expected " ++ (show expected) ++ " but found end of input.")

-- E -> F E'
-- E' -> F E' | ε
-- F -> λ x . E | x | ( E )

parseE :: TokenParser Exp
parseE = do
  f <- parseF
  es <- parseE'
  case es of
    [] -> return f
    _ -> return $ App (f : es)
  where
    parseE' :: TokenParser [Exp]
    parseE' = do
      more <- peek startsE
      if not more then return []
      else do
        f <- parseF
        es <- parseE'
        return $ f : es
    parseF = condMOrElse [
      (peek ((==) TLambda), parseLambda),
      (peek ((==) TLParen), parseParens)] parseVar
    startsE TLParen = True
    startsE TLambda = True
    startsE (TAtom _) = True
    startsE _ = False
    parseLambda = do
      _ <- consume TLambda
      x <- parseAtom
      _ <- consume TDot
      e <- parseE
      return (Lambda x e)
    parseParens = do
      _ <- consume TLParen
      res <- parseE
      _ <- consume TRParen
      return res

parse :: TokenParser Exp
parse =
  Parser(\s ->
    case (runParser parseE s) of
      ParseError err -> ParseError err
      p@(Parsed _ []) -> p
      Parsed exp leftover ->
        ParseError ("Parsed " ++ (show exp) ++
                    " Leftover input: " ++ (show leftover)))

ppResult :: ParseResult a Exp -> String
ppResult (Parsed expression _) = pp expression
ppResult (ParseError err) = "Error: " ++ err

ppExp :: String -> String
ppExp = ppResult . runParser parse . tokenize

main :: IO ()
main = do
  -- TODO: ASCII art of a lambp.
  putStrLn "Welcome to the lambp interpreter :3"
  putStrLn "(Ctrl-C to exit)"
  repl
  where
    repl = do
      line <- getLine
      putStrLn (ppExp line) >> repl
