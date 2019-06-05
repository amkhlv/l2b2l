module NaiveSExp(
  SExp(Sym, Keyword, Value, Comment, SExp),
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
import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe
import qualified System.Console.ANSI.Codes as ANSI
import           System.Console.ANSI.Types

data SExp = Sym String
          | Keyword String
          | Value String
          | Comment String
          | SExp [SExp]

instance Show SExp where
  show (Keyword x) = ANSI.setSGRCode [ SetColor Foreground Dull Yellow ] ++ "#:" ++ x ++ ANSI.setSGRCode [ Reset ]
  show (Value x) = ANSI.setSGRCode [ SetColor Foreground Dull Blue ] ++ "｢" ++ x ++ "｣" ++ ANSI.setSGRCode [ Reset ]
  show (Comment x) = ANSI.setSGRCode [ SetColor Foreground Dull Yellow ] ++ "«" ++ x ++ "»" ++ ANSI.setSGRCode [ Reset ]
  show (Sym x) = ANSI.setSGRCode [ SetColor Foreground Vivid Green ] ++ x ++ ANSI.setSGRCode [ Reset ]
  show (SExp x) = ANSI.setSGRCode [ SetColor Foreground Vivid Yellow ] ++ "(" ++ ANSI.setSGRCode [ Reset ]
    ++ unwords (map show x)
    ++ ANSI.setSGRCode [ SetColor Foreground Vivid Yellow ] ++ ")" ++ ANSI.setSGRCode [ Reset ]

valueParser :: String -> Parser SExp
valueParser xs = try (Value <$> (char '"' >> return (reverse xs)))
  <|> try (char '\\' >> anyChar >>= \c -> valueParser ('\\':(c:xs)))
  <|> (anyChar >>= \c -> valueParser (c:xs))

commentParser :: String -> Parser SExp
commentParser cs =
  try (Comment <$> (newline >> return (reverse cs)))
  <|> (anyChar >>= \c -> commentParser (c:cs))

symbolParser :: Parser String
symbolParser = many1 $ noneOf "@()[]{} \n\",'`;|\\"

leafParser :: Parser SExp
leafParser = removeSpacesAround $ try (char '"' >> valueParser "")
  <|> try atExpParser
  <|> try (char ';' >> commentParser "")
  <|> try (Keyword <$> (string "#:" >> symbolParser))
  <|> fmap Sym symbolParser

removeSpacesAround :: Parser a -> Parser a
removeSpacesAround p = skipMany space >> p >>= (skipMany space >>) . return

listOfExpParser :: Parser [SExp]
listOfExpParser = try (between (char '(') (char ')') (many (try atExpParser <|> sExpParser)))
  <|> between (char '[') (char ']') (many (try atExpParser <|> sExpParser))

sExpParserTight :: Parser SExp
sExpParserTight =
  try (string "`" >> (SExp . (Sym "quasiquote" :) <$> listOfExpParser))
  <|> try (string "," >> (SExp <$> (try ((Sym "unquote" :) <$> listOfExpParser)
                                    <|>  ((\x -> [Sym "unquote", x]) <$> leafParser))))
  <|> try (string "'" >> (try (SExp . (Sym "quote" :) <$> listOfExpParser)
                          <|> Sym <$> symbolParser ))
  <|> try leafParser
  <|> SExp <$> listOfExpParser

sExpParser :: Parser SExp
sExpParser =  removeSpacesAround sExpParserTight

atExpParserTight :: Parser SExp
atExpParserTight = 
  char '@' >> sExpParserTight >>= cmdParser >>= interpolatedTextParser >>= literalTextParser

atExpParser :: Parser SExp
atExpParser = removeSpacesAround atExpParserTight

cmdParser :: SExp -> Parser SExp
cmdParser (Sym x) =
  try (SExp . (Sym x:) <$> between (char '[') (char ']') (many (sExpParser <|> atExpParser)))
  <|> return (Sym x)
cmdParser x = return x

interpolator :: [SExp] -> String -> Parser [SExp]
interpolator xs acc = 
  try (atExpParserTight >>=
       (\x -> interpolator (if acc == "" then x:xs else x:(Value $ reverse acc):xs) ""))
  <|> try (between (char '{') (char '}') (interpolator (Value (reverse $ '{':acc):xs) "") >>=
           flip interpolator "}")
  <|> try (noneOf "{}" >>= (\c -> interpolator xs (c:acc)))
  <|> return (if acc == "" then xs else (Value $ reverse acc):xs)

revmrg :: [SExp] -> [SExp]
revmrg = revmrg' [] where
  revmrg' acc [] = acc
  revmrg' acc (Value a : Value b : rst) = revmrg' acc (Value  (b ++ a) : rst)
  revmrg' acc (y:ys) = revmrg' (y:acc) ys
 
interpolatedTextParser :: SExp -> Parser SExp
interpolatedTextParser (Comment x) = return (Comment x)
interpolatedTextParser (SExp sexps) =
  try (SExp . (sexps ++) . revmrg <$> between (char '{') (char '}') (interpolator [] ""))
  <|> return (SExp sexps)
interpolatedTextParser x =
  try (SExp . revmrg <$> between (char '{') (char '}') (interpolator [x] "")) <|> return x


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

literator :: String -> Parser SExp
literator acc = do
  dataHereDelim <- getStop ""
  Value <$> manyTill anyChar (string dataHereDelim)

literalTextParser :: SExp -> Parser SExp
literalTextParser (Comment x) = return (Comment x)
literalTextParser (SExp sexps) =
  try ((\v -> SExp (sexps ++ [v])) <$> (char '|' >> literator "")) <|> return (SExp sexps)
literalTextParser x = try ((\v -> SExp [x,v]) <$> (char '|' >> literator "")) <|> return x

scrblParser :: Parser [Either SExp String]
scrblParser = scrblParser' [] [] where
  scrblParser' acc xs =
    try ( atExpParserTight >>= \x -> scrblParser' [] (Left x : Right (reverse acc) : xs))
    <|> (anyChar >>= \c -> scrblParser' (c:acc) xs)
    <|> return (reverse (Right (reverse acc) : xs))
