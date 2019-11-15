module BystroToc where

import qualified Options.Applicative as O
import           Control.Monad (when)
import           Data.Semigroup ((<>))
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.ParserCombinators.Parsec
import           NaiveSExp
import           SExp2LaTeX
import qualified BystroConf as BConf
import           System.Console.ANSI

data Clops = Clops
  { listHTMLs :: Bool
   , name :: Maybe String
   , lim :: Int
  }

data Section = Section 
  { level :: Int
  , title :: String
  , summary :: Maybe String
  }

scribbleName :: O.Parser (Maybe String)
scribbleName = O.optional $ O.strOption (O.short 's' <> O.metavar "NAME")
listHTMLsFlag :: O.Parser Bool
listHTMLsFlag = O.switch (O.long "htmls" <> O.help "list HTMLs" )
levelLimit :: O.Parser Int
levelLimit = O.option O.auto (O.short 'L' <> O.value 10 <> O.metavar "LEVEL_LIMIT")
parser :: O.Parser Clops
parser = Clops <$> listHTMLsFlag <*> scribbleName <*> levelLimit
opts :: O.ParserInfo Clops
opts = O.info ( parser O.<**> O.helper) (O.fullDesc <> O.progDesc "print table of contents")

printHTMLs :: String -> IO ()
printHTMLs name = do
  x <- readFile $ name ++ "/index.html"
  let tags = parseTags x
      toctags = filter (tagOpenAttrNameLit "a" "class" (== "toptoclink")) tags
  sequence_ [putStrLn $ name ++ "/" ++ fromAttrib "href" t | t <- toctags ]

printSection :: Section -> Int -> IO ()
printSection sec lim = when (level sec < lim) $ do
  setSGR [SetColor Foreground Dull Green]
  putStr $ concat (replicate (1 + level sec) "--")
  putStr " "
  setSGR [Reset]
  case level sec of 
    0 -> setSGR [SetColor Foreground Vivid Yellow]
    1 -> return ()
    _ -> setSGR [SetColor Foreground Vivid Blue]
  putStr $ title sec
  setSGR [Reset]
  case summary sec of
    Just s -> putStr $ "(" ++ s ++ ")"
    Nothing -> return () 
  putStrLn ""

procElem' :: String -> [SExp] -> String
procElem' acc [] = acc
procElem' acc (Str x : xs) = procElem' (acc ++ x) xs
procElem' acc (x:xs) = procElem' (acc ++ "❄") xs
procElem :: [SExp] -> String
procElem = procElem' []
slide2TOC' :: [Section] -> [Either String SExp] -> [Section]
slide2TOC' acc [] = acc
scrbl2TOC' :: [Section] -> [Either String SExp] -> [Section]
scrbl2TOC' acc [] = reverse acc
scrbl2TOC' acc (Right (SExp (Sym "page": Str ttl: Keyword "tag": Str lbl: _)) : xs) = 
  scrbl2TOC' (Section 0 ttl Nothing:acc) xs 
scrbl2TOC' acc (Right (SExp (Sym "page": SExp (Sym "elem" : ys) : Keyword "tag": Str lbl: _)) : xs) = 
  scrbl2TOC' (Section 0 (procElem ys) Nothing:acc) xs 
scrbl2TOC' acc (Right (SExp (Sym "subpage": Int n: Str ttl: Keyword "tag": Str lbl: _)) : xs) = 
  scrbl2TOC' (Section (fromInteger n) ttl Nothing:acc) xs 
scrbl2TOC' acc (Right (SExp (Sym "subpage": Int n: SExp (Sym "elem" : ys) : Keyword "tag": Str lbl: _)) : xs) = 
  scrbl2TOC' (Section (fromInteger n) (procElem ys) Nothing:acc) xs 
scrbl2TOC' acc (Right (SExp (Sym "slide": Str ttl: cs)) : xs) = 
   scrbl2TOC' (scrbl2TOC' [] (Right <$> cs) ++ Section 0 ttl Nothing:acc) xs
scrbl2TOC' acc (Right (SExp (Sym "section": cs)) : xs) = 
  scrbl2TOC' (Section 1 (case head cs of {Str ttl -> ttl ;  SExp (Sym "elem" : ys) -> procElem ys}) Nothing: acc) xs
scrbl2TOC' acc (Right (SExp (Sym "subsection": cs)) : xs) = 
  scrbl2TOC' (Section 2 (case head cs of {Str ttl -> ttl ;  SExp (Sym "elem" : ys) -> procElem ys}) Nothing: acc) xs
scrbl2TOC' acc (Right (SExp (Sym "subsubsection": cs)) : xs) = 
  scrbl2TOC' (Section 3 (case head cs of {Str ttl -> ttl ;  SExp (Sym "elem" : ys) -> procElem ys}) Nothing: acc) xs
scrbl2TOC' acc (x:xs) = scrbl2TOC' acc xs
scrbl2TOC :: [Either String SExp] -> [Section]
scrbl2TOC = scrbl2TOC' [] 
  
printName :: String -> IO ()
printName nm = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStrLn nm
  setSGR [Reset]

printTOC :: String -> Int -> IO ()
printTOC name lim = do
  scrbl <- readFile $ name ++ ".scrbl"
  case parse scrblParser "" scrbl of
    Left _ -> putStrLn "ERROR"
    Right x -> sequence_ [printSection sec lim | sec <- scrbl2TOC x]

main = do
  options <- O.execParser opts
  case name options of
    Just nm -> if listHTMLs options then printHTMLs nm else printTOC nm (lim options)
    Nothing -> do
      confs <- BConf.getScribblingConfs
      sequence_ [
        do
          printName $ BConf.name x
          printTOC ( BConf.name x ) (lim options)
        | x <- confs
        ]

