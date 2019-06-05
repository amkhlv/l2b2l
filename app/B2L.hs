module B2L where

import Text.ParserCombinators.Parsec
import NaiveSExp

main :: IO ()
main = do
  scrbl <- getContents
  putStrLn $ case (parse scrblParser "" scrbl) of
    Left _  -> "ERROR"
    Right x -> show x
