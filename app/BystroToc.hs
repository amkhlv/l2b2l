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
levelLimit = O.option O.auto (O.short 'L' <> O.value 2 <> O.metavar "LEVEL_LIMIT")
parser :: O.Parser Clops
parser = Clops <$> listHTMLsFlag <*> scribbleName <*> levelLimit
opts :: O.ParserInfo Clops
opts = O.info ( parser O.<**> O.helper) (O.fullDesc <> O.progDesc "print table of contents")

printHTMLs :: String -> IO ()
printHTMLs name = do
  x <- readFile $ name ++ "/index.html"
  let tags = parseTags x
      toctags = filter (tagOpenAttrNameLit "a" "class" (== "bystro-toc-section")) tags
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
    Just s -> do
      setSGR [SetColor Foreground Dull Blue]
      putStr $ " ← " ++ s 
      setSGR [Reset]
    Nothing -> return () 
  putStrLn ""

procSecDecl :: Int -> [SExp] -> Section
procSecDecl level (Keyword _ : x : xs) = procSecDecl level xs
procSecDecl level (Str ttl : xs) = Section level ttl Nothing
procSecDecl level (SExp (Sym "elem" : ys) : xs) = Section level (procElem ys) Nothing
procElem' :: String -> [SExp] -> String
procElem' acc [] = acc
procElem' acc (Str x : xs) = procElem' (acc ++ x) xs
procElem' acc (x:xs) = procElem' (acc ++ "❄") xs
procElem :: [SExp] -> String
procElem = procElem' []
addSummary :: Section -> [SExp] -> Section
addSummary (Section level title Nothing) xs = Section level title (Just $ procElem xs)
slide2TOC' :: [Section] -> [Either String SExp] -> [Section]
slide2TOC' acc [] = acc
scrbl2TOC' :: [Section] -> [Either String SExp] -> [Section]
scrbl2TOC' acc [] = reverse acc
scrbl2TOC' acc (Right (SExp (Sym "page": ys)) : xs) = scrbl2TOC' (procSecDecl 0 ys:acc) xs
scrbl2TOC' acc (Right (SExp (Sym "subpage": Int n: ys)) : xs) =  scrbl2TOC' (procSecDecl (fromInteger n) ys : acc) xs
scrbl2TOC' acc (Right (SExp (Sym "slide": Str ttl: cs)) : xs) =
   scrbl2TOC' (scrbl2TOC' [] (Right <$> cs) ++ Section 0 ttl Nothing:acc) xs
scrbl2TOC' acc (Right (SExp (Sym "section": cs)) : xs) = scrbl2TOC' (procSecDecl 1 cs : acc) xs
scrbl2TOC' acc (Right (SExp (Sym "subsection": cs)) : xs) = scrbl2TOC' (procSecDecl 2 cs : acc) xs
scrbl2TOC' acc (Right (SExp (Sym "subsubsection": cs)) : xs) = scrbl2TOC' (procSecDecl 2 cs : acc) xs
scrbl2TOC' (z:zs) (Right (SExp (Sym "summary" : ys)): xs) = scrbl2TOC' (addSummary z ys : zs) xs
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

