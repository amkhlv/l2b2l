module L2B where

import Data.Maybe
import Text.ParserCombinators.Parsec
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Pretty
import qualified  Text.PrettyPrint.Free as PP

sections = ["section", "subsection", "subsubsection"]

-- this configures printing of the LaTeX code
prntLT :: LaTeX -> String
prntLT ltx = T.unpack $ render ltx
-- prntLT ltx = let
--   d = docLaTeX ltx
--   sd = PP.renderPretty 1.0 200 d in
--   PP.displayS sd ""

btArg :: TeXArg -> Text
btArg (FixArg ltx) = bt False ltx
btArg (OptArg ltx) = bt False ltx
btArg (MOptArg ltxs) = T.concat $ bt False <$> ltxs
    

-- functions below are needed to convert \label into @label and @tag

findLabel :: LaTeX -> Maybe Text
findLabel ltx = case ltx of
  TeXComm "label" [FixArg (TeXRaw lbl)] -> Just lbl
  TeXSeq l1 l2 -> case l1 of
    TeXComm "label" [FixArg (TeXRaw lbl)] -> Just lbl
    _ -> findLabel l2
  _ -> Nothing

getLabelAndRest :: LaTeX -> Maybe (Text, LaTeX) 
getLabelAndRest = getLabelAndRest' TeXEmpty
getLabelAndRest' acc ltx = case ltx of
  TeXComm "label" [FixArg (TeXRaw lbl)] -> Just (lbl, acc)
  TeXSeq l1 l2 -> case l1 of
    TeXComm "label" [FixArg (TeXRaw lbl)] -> Just (lbl, acc <> l2)
    _ -> getLabelAndRest' (acc <> l1) l2
  x -> Nothing

unLabel :: LaTeX -> LaTeX
unLabel (TeXComm "label" _) = TeXEmpty
unLabel (TeXSeq (TeXComm "label" _) x) = x
unLabel (TeXSeq ll lr) = TeXSeq ll (unLabel lr)
unLabel x = x

splitOnAmpersand  = splitOnAmpersand' TeXEmpty
splitOnAmpersand' :: LaTeX -> LaTeX -> (LaTeX,LaTeX)
needToDrop :: LaTeX -> Bool
needToDrop TeXEmpty = True
needToDrop (TeXRaw x) = T.null (T.dropWhile (\y -> (y == '\n') || (y == ' ')) x)
needToDrop _ = False
chomp t = T.dropWhileEnd (\y -> (y == '\n') || (y == ' ')) $ T.dropWhile (\y -> (y == '\n') || (y == ' ')) t
chompStr s = T.unpack $ chomp $ T.pack s
splitOnAmpersand' acc (TeXRaw x) = case T.breakOnAll (T.pack "&") x of
  [(t1,t2)] -> (
    if needToDrop acc then TeXRaw $ T.dropWhile (\y -> (y == '\n') || (y == ' ')) t1 else acc <> TeXRaw t1,
    TeXRaw (chomp (T.drop 1 t2))
    )
  [] -> (TeXEmpty, acc <> TeXRaw x)
splitOnAmpersand' acc (TeXSeq (TeXRaw x) y) = case T.breakOnAll (T.pack "&") x of
  [(t1,t2)] -> (
    if needToDrop acc then TeXRaw $ T.dropWhile (\y -> (y == '\n') || (y == ' ')) t1 else acc <> TeXRaw t1,
    TeXRaw (chomp (T.drop 1 t2)) <> y
    )
  _ -> splitOnAmpersand' (acc <> TeXRaw x) y
splitOnAmpersand' acc (TeXSeq x y) = splitOnAmpersand' (acc <> x) y
splitOnAmpersand' acc x = (TeXEmpty, acc <> x)
dropNonumber :: LaTeX -> LaTeX
dropNonumber (TeXCommS "nonumber") = TeXEmpty
dropNonumber (TeXSeq la lb) = TeXSeq (dropNonumber la) (dropNonumber lb)
dropNonumber x = x
dropTrailingNewline :: LaTeX -> LaTeX
dropTrailingNewline (TeXRaw x) = TeXRaw $ T.dropWhileEnd (== '\n') x
dropTrailingNewline (TeXSeq l1 l2) = TeXSeq l1 $ dropTrailingNewline l2
dropTrailingNewline x = x

data LineWithLabel = LineWithLabel { lineTeX :: LaTeX , label :: Maybe Text }

alignedLines :: LaTeX -> LaTeX -> [LineWithLabel]
alignedLines TeXEmpty TeXEmpty = []
alignedLines TeXEmpty (TeXLineBreak mm b) = []
alignedLines acc      (TeXLineBreak mm b) = [LineWithLabel (unLabel acc) (findLabel acc)]
alignedLines acc      (TeXSeq (TeXLineBreak mm b) lb) =
  (LineWithLabel (unLabel acc) (findLabel acc)) : (alignedLines TeXEmpty lb)
alignedLines acc (TeXSeq la lb) = alignedLines (acc <> la) lb
alignedLines acc x = let ax = acc <> x in [LineWithLabel (unLabel ax) (findLabel ax)]

align :: [LineWithLabel] -> String
align [] = ""
align (LineWithLabel ln (Just lbl) : rest) = let s = splitOnAmpersand ln in
  "`(" ++ case fst s of {TeXEmpty -> "\"\"\n" ; x -> "@,f{" ++ chompStr (prntLT $ fst s) ++ "}\n"} 
  ++ "   @,f{" ++ chompStr (prntLT (dropTrailingNewline $ snd s)) ++ "}\n"
  ++ "   @,label{" ++ T.unpack lbl ++ "})\n" ++ align rest
align (LineWithLabel ln Nothing : rest) = let s = splitOnAmpersand ln in
  "`(" ++ case fst s of {TeXEmpty -> "\"\"\n" ; x -> "@,f{" ++ chompStr (prntLT $ fst s) ++ "}\n"}
  ++ "   @,f{" ++ chompStr (prntLT (dropTrailingNewline $ snd s)) ++ "}\n"
  ++ "   \"\")\n" ++ align rest

bt :: Bool -> LaTeX -> Text
bt math TeXEmpty = T.pack ""
bt math (TeXRaw txt) = txt
bt True (TeXComm com args) = T.pack $ prntLT (TeXComm com args)
bt math (TeXComm "hspace" [FixArg (TeXRaw n)]) = T.concat [T.pack "@hspace[", (T.takeWhile isDigit n), T.pack "]"]
bt math (TeXComm com args) = T.concat $ (T.pack <$> ["@", com, "{"]) ++ (btArg <$> args) ++ [T.pack "}"]
bt True (TeXCommS com) = T.pack $ prntLT (TeXCommS com)
bt math (TeXCommS com) = T.pack $ "@" ++ com
bt math (TeXEnv com args ltx)
  | com == "equation" = case getLabelAndRest ltx of
      Just (lbl, rst) -> T.concat [T.pack "@e[#:label \"",
                                   lbl,
                                   T.pack "\"]{\n   ", chomp $ T.pack $ prntLT rst, T.pack "\n   }"]
      Nothing         -> T.concat (T.pack <$> ["@e{\n   ", chompStr $ prntLT ltx, "\n   }"])
  | com == "align" = T.pack $ "@(align\n  r.l.n\n  " ++ (align $ alignedLines TeXEmpty (dropNonumber ltx)) ++ ")"
  | otherwise = 
    T.concat $ (T.pack <$> ["@", com, "{"]) ++ [bt math ltx] ++ [T.pack "}"]
bt math (TeXMath mtype ltx)
  | mtype == Parentheses || mtype == Dollar =
    T.concat $ T.pack <$> ["@f{", chompStr $ prntLT ltx, "}"]
  | otherwise = 
    T.concat $ T.pack <$> ["@e{\n   ", chompStr $ prntLT ltx, "\n   }"]
bt math (TeXLineBreak mm b) = T.pack "@linebreak[]"
bt True (TeXBraces ltx) = T.pack (prntLT (TeXBraces ltx))
bt False (TeXBraces (TeXSeq (TeXCommS "bf") ltx)) = T.concat $ [T.pack "@bold{"] ++ [bt False ltx] ++ [T.pack "}"]
bt False (TeXBraces (TeXSeq (TeXCommS "it") ltx)) = T.concat $ [T.pack "@italic{"] ++ [bt False ltx] ++ [T.pack "}"]
bt False (TeXBraces (TeXSeq (TeXCommS "em") ltx)) = T.concat $ [T.pack "@spn[attn]{"] ++ [bt False ltx] ++ [T.pack "}"]
bt math (TeXBraces ltx) = bt math ltx
bt math (TeXSeq la lb) = T.append (bt math la) (bt math lb)
bt math (TeXComment txt) = T.append (T.pack "@;") txt

printSection :: String -> [TeXArg] -> LaTeX -> IO ()
printSection x args lb = case getLabelAndRest lb of
  Just (lbl, rst) -> do
    TIO.putStr . T.concat $
      (T.pack <$> ["@", x, "[#:tag \""]) ++ [lbl, T.pack "\"]{"] ++ [btArg arg | arg <- args] ++ [T.pack "}"]
    printBT rst
  Nothing -> do
    TIO.putStr . T.concat $ (T.pack <$> ["@", x, "{"]) ++ [btArg arg | arg <- args] ++ [T.pack "}"]
    printBT lb

printBT :: LaTeX -> IO ()
printBT (TeXEnv "document" _ rst) = printBT rst
printBT (TeXSeq (TeXComm x args) lb) | elem x sections = printSection x args lb
printBT (TeXSeq la lb) = TIO.putStr (bt False la) >> printBT lb
printBT x = TIO.putStr (bt False x)

main :: IO ()
main = do
  input <- getContents
  case (parseLaTeX $ T.pack input) of
    Left err -> putStrLn "Error parsing LaTeX"
    -- Right ltx -> putStrLn $ show ltx
    Right ltx -> printBT ltx
