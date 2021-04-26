{-# LANGUAGE OverloadedStrings #-}

module B2Pan where

import qualified Data.Text as T
import           Text.ParserCombinators.Parsec
import           NaiveSExp
import qualified Options.Applicative as O
import           Text.LaTeX
import qualified Data.Text.IO as TIO
import           SExp2Pandoc
import           Text.Pandoc
import           Text.Pandoc.Writers

data Clops = Clops {
  dumpSExp :: Bool
}

cloparser :: O.Parser Clops
cloparser = Clops
  <$> O.switch ( O.short 's' <> O.help "show SExp" )


dispatch :: Clops -> IO ()
dispatch clops = do
  scrbl <- getContents
  case parse scrblParser "" scrbl of
    Left _ -> putStrLn "ERROR"
    Right x -> if dumpSExp clops then print x else do
      txt <- runIO $ writeMarkdown def (scrbl2Pan x) 
      case txt of
        Left e -> print txt
        Right t -> putStr $ T.unpack t

main :: IO ()
main = dispatch =<< O.execParser opts
  where
    opts = O.info (O.helper <*> cloparser) ( O.fullDesc <> O.progDesc "BystroTeX to Pandoc converter" <> O.header "convert .scrbl to Markdown" )

