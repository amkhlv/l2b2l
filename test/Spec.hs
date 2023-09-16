import Text.ParserCombinators.Parsec
import NaiveSExp

main :: IO ()
main = do
  scrbl <- readFile "test.scrbl"
  putStrLn $ case parse scrblParser "" scrbl of
    Left _  -> "ERROR"
    Right x -> show x
  putStrLn "finished test.scrbl"
