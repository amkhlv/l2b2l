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
import           Text.Pandoc.Options
import qualified Data.ByteString.Lazy as BS
import qualified Text.Pandoc.Builder as PB

data Clops = Clops {
  dumpSExp :: Bool
  , outputFile :: Maybe String
}

cloparser :: O.Parser Clops
cloparser = Clops
  <$> O.switch ( O.short 's' <> O.help "show SExp" )
  <*> O.optional ( O.option O.str (O.short 'o' <> O.metavar "OUTPUT" <> O.help "output file name (type derived from extension)"))


dispatch :: Clops -> IO ()
dispatch clops = do
  scrbl <- getContents
  case parse scrblParser "" scrbl of
    Left _ -> putStrLn "ERROR"
    Right x -> if dumpSExp clops 
      then 
      print x 
      else case outputFile clops of
        Nothing -> do
          txt <- runIO $ writeNative def (scrbl2Pan x) 
          case txt of
            Left e -> print txt
            Right t -> putStr $ T.unpack t
        Just f  
          | length f > 2 && reverse (take 3 $ reverse f) == ".md" ->
            do
              txt <- runIO $ writeMarkdown (def { writerExtensions = pandocExtensions }) (scrbl2Pan x) 
              case txt of
                Left e -> print txt
                Right t -> writeFile f $ T.unpack t 
          | length f > 4 && reverse (take 5 $ reverse f) == ".html" ->
            do
              txt <- runIO $ writeHtml5String def (scrbl2Pan x) 
              case txt of
                Left e -> print txt
                Right t -> writeFile f $ T.unpack t 
          | length f > 4 && reverse (take 5 $ reverse f) == ".docx" ->
            do
              txt <- runIO $ writeDocx def (scrbl2Pan x)
              case txt of
                Left e -> print txt
                Right t -> BS.writeFile f t 
          | otherwise -> putStrLn "unrecognized file extension"

main :: IO ()
main = dispatch =<< O.execParser opts
  where
    opts = O.info (O.helper <*> cloparser) ( O.fullDesc <> O.progDesc "BystroTeX to Pandoc converter" <> O.header "convert .scrbl to Markdown" )

