module NaiveSExp(
  SExp(Sym, Keyword, Str, Int, Dbl, Bln, Comment, SExp),
  sExpParser,
  sExpParserTight,
  atExpParser,
  atExpParserTight,
  scrblParser
  ) where

{-
  This is *not* a Racket parser !
-}

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as TKN
import qualified Text.Parsec.Language as LANG
import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import qualified System.Console.ANSI.Codes as ANSI
import           System.Console.ANSI.Types
import qualified Control.Monad.IO.Class as MIO
import           Debug.Trace

data SExp = Sym String
          | Keyword String
          | Str String
          | Int Integer
          | Dbl Double
          | Bln Bool
          | Comment String
          | SExp [SExp]

instance Show SExp where
  show (Keyword x) = ANSI.setSGRCode [ SetColor Foreground Dull Yellow ] ++ "#:" ++ x ++ ANSI.setSGRCode [ Reset ]
  show (Str x) = ANSI.setSGRCode [ SetColor Foreground Dull Blue ] ++ "｢" ++ x ++ "｣" ++ ANSI.setSGRCode [ Reset ]
  show (Int x) = ANSI.setSGRCode [ SetColor Foreground Dull Blue ] ++ show x ++ ANSI.setSGRCode [ Reset ]
  show (Dbl x) = ANSI.setSGRCode [ SetColor Foreground Dull Blue ] ++ show x ++ ANSI.setSGRCode [ Reset ]
  show (Bln x) = ANSI.setSGRCode [ SetColor Foreground Dull Red ] ++ if x then "#t" else "#f" ++ ANSI.setSGRCode [ Reset ]
  show (Comment x) = ANSI.setSGRCode [ SetColor Foreground Dull Yellow ] ++ "«" ++ x ++ "»" ++ ANSI.setSGRCode [ Reset ]
  show (Sym x) = ANSI.setSGRCode [ SetColor Foreground Vivid Green ] ++ x ++ ANSI.setSGRCode [ Reset ]
  show (SExp x) = ANSI.setSGRCode [ SetColor Foreground Vivid Yellow ] ++ "(" ++ ANSI.setSGRCode [ Reset ]
    ++ unwords (map show x)
    ++ ANSI.setSGRCode [ SetColor Foreground Vivid Yellow ] ++ ")" ++ ANSI.setSGRCode [ Reset ]

msg :: String -> Parser a -> Parser a
msg x p = do
  trace ( ANSI.setSGRCode [ SetColor Foreground Vivid Red ] ++ x ++ ANSI.setSGRCode [ Reset ]) $ return ()
  p

racketDef = LANG.emptyDef {
  TKN.commentLine = ";"
  , TKN.nestedComments = False
  , TKN.identLetter    = alphaNum <|> noneOf "@()[]{} \n\",'`;|\\"
  }
racket = TKN.makeTokenParser racketDef

sym :: Parser String
sym = many1 $ noneOf "@()[]{} \n\",'`;|\\"

commentParser :: String -> Parser SExp
commentParser cs =
  try (Comment <$> (newline >> return (reverse cs)))
  <|> (anyChar >>= \c -> commentParser (c:cs))

removeSpacesAround :: Parser a -> Parser a
removeSpacesAround p = skipMany space >> p >>= (skipMany space >>) . return

leafParserTight :: Parser SExp
leafParserTight =
  try (Str <$> TKN.stringLiteral racket)
  <|> try (Dbl <$> TKN.float racket)
  <|> try (Int <$> TKN.integer racket)
  <|> try (Bln <$> (try (string "#t" >> return True) <|> (string "#f" >> return False)))
  <|> try (char ';' >> commentParser "")
  <|> try (Keyword <$> (string "#:" >> sym))
  <|> Sym <$> sym

leafParser = removeSpacesAround leafParserTight

-- [ (sexp|atexp) ...]
listOfExpParser :: Parser [SExp]
listOfExpParser = let p = many $ try atExpParser <|> sExpParser
                  in try (between (char '(') (char ')') p) <|> between (char '[') (char ']') p

symThenListParser :: Parser [SExp]
symThenListParser = sym >>= (\s -> (\xs -> Sym "unquote" : Sym s : xs) 
                                    <$> 
                                    between (char '[') (char ']') (many $ try atExpParser <|> sExpParser))
symThenBracesParser :: Parser [SExp]
symThenBracesParser = sym >>= (\s -> string "{}" >> return [Sym s])

sExpParserTight :: Parser SExp
sExpParserTight =
  try (string "`" >> (SExp . (Sym "quasiquote" :) <$> listOfExpParser))
  <|> try (string "," >> (SExp <$> (try ((Sym "unquote" :) <$> listOfExpParser)
                                    <|> try symThenListParser
                                    <|> try symThenBracesParser
                                    <|>  ((\x -> [Sym "unquote", x]) <$> leafParserTight))))
  <|> try (string "'" >> (try (SExp . (Sym "quote" :) <$> listOfExpParser)
                          <|> Sym <$> sym))
  <|> try leafParserTight
  <|> SExp <$> listOfExpParser

sExpParser :: Parser SExp
sExpParser =  removeSpacesAround sExpParserTight

atExpParserTight :: Parser SExp
atExpParserTight = char '@' >> sExpParserTight >>= cmdParser >>= interpolatedTextParser

atExpParser :: Parser SExp
atExpParser = removeSpacesAround atExpParserTight

cmdParser :: SExp -> Parser SExp
cmdParser (Sym x) =
  try (SExp . (Sym x:) <$> between (char '[') (char ']') (many (try sExpParser <|> atExpParser)))
  <|> return (Sym x)
cmdParser x = return x

interpolator :: [SExp] -> String -> Parser [SExp]
interpolator xs acc = 
  try (atExpParserTight >>=
       (\x -> interpolator (if acc == "" then x:xs else x:(Str $ reverse acc):xs) ""))
  <|> try (between (char '{') (char '}') (interpolator (Str (reverse $ '{':acc):xs) "") >>=
           flip interpolator "}")
  <|> try (noneOf "{}" >>= (\c -> interpolator xs (c:acc)))
  <|> return (if acc == "" then xs else (Str $ reverse acc):xs)

revmrg :: [SExp] -> [SExp]
revmrg = revmrg' [] where
  revmrg' acc [] = acc
  revmrg' acc (Str a : Str b : rst) = revmrg' acc (Str  (b ++ a) : rst)
  revmrg' acc (y:ys) = revmrg' (y:acc) ys
 
interpolatedTextParser :: SExp -> Parser SExp
interpolatedTextParser (Comment x) = return (Comment x)
interpolatedTextParser (SExp sexps) =
  try (SExp . (sexps ++) . revmrg <$> between (char '{') (char '}') (interpolator [] ""))
  <|> return (SExp sexps)
interpolatedTextParser x =
  try (SExp . revmrg <$> between (char '{') (char '}') (interpolator [x] "")) <|> literalTextParser x


mirror :: Char -> Maybe Char
mirror c =
  let punctA = "->+="
      punctB = "-<+="
  in
    case elemIndex c punctA of
      Just n -> Just (punctB !! n)
      Nothing -> case elemIndex c punctB of
        Just n -> Just (punctA !! n)
        Nothing -> Nothing

getStop :: String -> Parser String
getStop acc = try (char '{' >> return ( "}" ++ acc ++ "|"))
  <|> (do
          c <- anyChar
          case mirror c of
            Just x -> getStop (x:acc)
            Nothing -> pzero)

literator :: Parser SExp
literator = do
  dataHereDelim <- getStop ""
  s <- manyTill anyChar (try $ string dataHereDelim)
  return $ Str s

literalTextParser :: SExp -> Parser SExp
literalTextParser (Comment x) = return (Comment x)
literalTextParser (SExp sexps) =
  try ((\v -> SExp (sexps ++ [v])) <$> (char '|' >> literator )) <|> return (SExp sexps)
literalTextParser x =
  try ((\v -> SExp [x,v]) <$> (char '|' >> literator)) <|> return x

scrblParser :: Parser [Either String SExp]
scrblParser = scrblParser' [] [] where
  scrblParser' acc xs =
    try (atExpParserTight >>= \x ->  scrblParser' [] (Right x : Left (reverse acc) : xs))
    <|> (anyChar >>= \c ->  scrblParser' (c:acc) xs)
    <|> return (reverse (Left (reverse acc) : xs))
