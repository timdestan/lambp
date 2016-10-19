{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad (ap)
import Data.Char (isAlphaNum)

data Exp = Var String
         | App [Exp]
         | Lambda String Exp
         deriving Show

data PPContext = PPTop | PPApp | PPLambdaBody

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
    enclose str = "(" ++ str ++ ")"

data ParseResult a = Parsed a String
                   | ParseError String
                   deriving (Functor, Show)

newtype Parser a = Parser(String -> ParseResult a)
                   deriving Functor

runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) s = f s

instance Monad Parser where
  Parser f >>= g =
    Parser(\s -> case (f s) of
      ParseError error  -> ParseError error
      Parsed a s -> runParser (g a) s)
  return a = Parser(\s -> Parsed a s)

instance Applicative Parser where
  pure a = Parser(\s -> Parsed a s)
  (<*>) = ap

-- Insert shrug emoji
canBeAtom :: Char -> Bool
canBeAtom 'λ' = False
canBeAtom '\\' = False
canBeAtom '.' = False
canBeAtom '(' = False
canBeAtom ')' = False
canBeAtom _ = True

parseAtom :: Parser String
parseAtom =
  fmap reverse (Parser (\str ->
    case str of
      (h:t) | canBeAtom h -> Parsed [h] t
      _ -> ParseError "Not an atom."))

parseVar :: Parser Exp
parseVar = do
  name <- parseAtom
  return (Var name)

peek :: (Char -> Bool) -> Parser Bool
peek pred = Parser(impl) where
  impl [] = Parsed False []
  impl str@(h:t) = Parsed (pred h) str

failWith :: String -> Parser a
failWith msg = Parser(impl) where
  impl _ = ParseError msg

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mc mt mf = do
  c <- mc
  t <- mt
  f <- mf
  if c then return t
       else return f

condM :: Monad m => [(m Bool, m a)] -> m a
condM [] = fail "Exhausted alternatives."
condM ((mc, mv) : t) = do
  c <- mc
  if c then mv
       else condM t

parseE :: Parser Exp
parseE = do
  f <- parseF
  moreExps <- peek startsE
  if moreExps then (fmap (\e -> App [f,e]) parseE)
              else return f
  where
    parseF = condM [
      (peek isLambda, parseLambda),
      (peek isLeftParen, parseParens),
      (peek (\x -> True), parseVar)]
    isLambda c = c == 'λ' || c == '\\'
    isLeftParen c = c == '('
    startsE c = isAlphaNum c || c == '(' || c == '\\'
    parseLambda = do
      _ <- consumeAnyChar ['λ', '\\']
      x <- parseAtom
      _ <- consumeChar '.'
      e <- parseE
      return (Lambda x e)
    parseParens = do
      _ <- consumeChar '('
      res <- parseE
      _ <- consumeChar ')'
      return res
    consumeChar c = consumeAnyChar [c]
    consumeAnyChar cs = Parser(\str ->
      case str of
        (h:t) | elem h cs -> Parsed () t
              | otherwise ->
                  ParseError ("Expected one of " ++ cs ++
                              " but found " ++ [h])
        _ -> ParseError ("Expected one of " ++ cs ++
                         " but found end of input."))

parse :: Parser Exp
parse =
  Parser(\s ->
    case (runParser parseE s) of
      ParseError err -> ParseError err
      p@(Parsed _ "") -> p
      Parsed exp leftover -> ParseError ("Parsed " ++ (show exp) ++
                                         " Leftover input: " ++ leftover))

ppResult :: ParseResult Exp -> String
ppResult (Parsed e _) = pp e
ppResult error = show error

ppExp :: String -> IO ()
ppExp str =
  let (result :: ParseResult Exp) = runParser parse str
      (prettyResult :: String) = ppResult result in
      putStrLn prettyResult

ppExps :: [String] -> IO ()
ppExps strs = sequence_ $ fmap ppExp strs

main :: IO ()
main = do
  ppExps [
    "x",
    "λx.λx.xy",
    "λx.λy.λw.zy",
    "λx.xx",
    "(λx.+1x)4",
    "λx.yx"]
