{-# LANGUAGE OverloadedStrings #-}

module B2L where

import qualified Data.Text as T
import           Text.ParserCombinators.Parsec
import           NaiveSExp
import qualified Options.Applicative as O
import           Text.LaTeX
import qualified Data.Text.IO as TIO
import           SExp2LaTeX

data Clops = Clops {
  dumpSExp :: Bool
  , fullDoc :: Bool
}

cloparser :: O.Parser Clops
cloparser = Clops <$> O.switch ( O.short 's' <> O.help "show SExp" )
                  <*> O.switch ( O.short 'f' <> O.help "make full LaTeX document, including preamble")

thePreamble :: Monad m => LaTeXT_ m
thePreamble = documentclass [] article

dispatch :: Clops -> IO ()
dispatch clops = do
  scrbl <- getContents
  case parse scrblParser "" scrbl of
    Left _ -> putStrLn "ERROR"
    Right x -> case (dumpSExp clops, fullDoc clops) of
      (True , _)     -> print x
      (False, True)  -> execLaTeXT (thePreamble >> document (scrbl2LaTeX x)) >>= (TIO.putStr . render)
      (False, False) -> execLaTeXT (scrbl2LaTeX x) >>= (TIO.putStr . render)

main :: IO ()
main = dispatch =<< O.execParser opts
  where
    opts = O.info (O.helper <*> cloparser) ( O.fullDesc <> O.progDesc "BystroTeX to LaTeX converter" <> O.header "convert .scrbl to .tex" )

