{-# LANGUAGE OverloadedStrings #-}

module SExp2Pandoc where

import           NaiveSExp
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Pandoc.Builder as PB
import qualified Text.Pandoc.Definition as PD
import qualified Data.Bifunctor as BF
import qualified Data.Either as DE

isComment :: SExp -> Bool
isComment (Comment _) = True
isComment _ = False

rmComments :: SExp -> SExp
rmComments (SExp xs) = SExp $ map rmComments (filter (not . isComment) xs)
rmComments x = x

unStr :: SExp -> String
unStr (Str x) = x

unQuote (SExp [Sym "unquote", x]) = x
unQuote (SExp (Sym "unquote":rest)) = SExp rest
unQuote x = x

sexp2Inlines :: SExp -> PB.Inlines
sexp2Inlines (Str x) = PB.text $ T.pack x
sexp2Inlines (SExp (Sym "bold":xs)) = PB.strong $ mconcat $ map sexp2Inlines xs
sexp2Inlines (SExp (Sym "italic":xs)) = PB.emph $ mconcat $ map sexp2Inlines xs
sexp2Inlines (SExp (Sym "hyperlink":Str href:xs)) = PB.link (T.pack href) "" (mconcat $ map (piece2Inlines . sexp2Piece) xs)
sexp2Inlines (SExp (Sym "f":xs)) = let f (Str s) = T.pack s in PB.math $ T.concat $ map f xs
sexp2Inlines (SExp (Sym "tt":xs)) = let f (Str s) = T.pack s in PB.code $ T.concat $ map f xs
sexp2Inlines (SExp (Sym "e":Keyword "label":Str l:rest)) = let f (Str s) = T.pack s in PB.displayMath $ T.concat $ map f rest
sexp2Inlines (SExp (Sym "e":rest)) = let f (Str s) = T.pack s in PB.displayMath $ T.concat $ map f rest
sexp2Inlines (SExp (Sym "linebreak":[])) = PB.linebreak


sexp2Piece :: SExp -> Either T.Text SExp
sexp2Piece = Right
piece2Inlines :: Either T.Text SExp -> PB.Inlines
piece2Inlines (Left t) = PB.text t
piece2Inlines (Right s) = sexp2Inlines s

acc2Para :: [Either T.Text SExp] -> PB.Blocks
acc2Para = PB.para . mconcat . map piece2Inlines . reverse

sexps2Blocks = splitIntoBlocks . linesplitter . map Right

mkrow :: SExp -> [PB.Blocks]
mkrow (SExp (Sym "list":cells)) = map (splitIntoBlocks . return . sexp2Piece) cells
mkrow (SExp (Sym "quote":cells)) = map (splitIntoBlocks . return . sexp2Piece) cells

splitIntoBlocks' :: PB.Blocks -> [Either T.Text SExp] -> [Either T.Text SExp] -> PB.Blocks
splitIntoBlocks' accblocks [] [] = accblocks
splitIntoBlocks' accblocks acc [] = splitIntoBlocks' (accblocks<>acc2Para acc) [] []
-- just finished a block :
splitIntoBlocks' accblocks [] (Left t:rest) 
-- if empty line detected :
  | T.null $ T.strip t = splitIntoBlocks' accblocks [] rest 
  | otherwise          = splitIntoBlocks' accblocks [Left t] rest
splitIntoBlocks' accblocks acc (Left t:rest) 
-- if empty line detected, then need to finalize the current block and push it into the accblocks :
  | T.null $ T.strip t = splitIntoBlocks' (accblocks<>acc2Para acc) [] rest
  | otherwise          = splitIntoBlocks' accblocks (Left t:Left " ":acc) rest
splitIntoBlocks' accblocks acc (Right (SExp (Sym "itemlist":Keyword "style":Sym "ordered":items)):rest) =
  let f (SExp (Sym "item":xs)) = sexps2Blocks xs 
  in splitIntoBlocks' (accblocks<>acc2Para acc<>PB.orderedList (map f items)) [] rest 
splitIntoBlocks' accblocks acc (Right (SExp (Sym "itemlist":items)):rest) =
  let f (SExp (Sym "item":xs)) = sexps2Blocks xs 
  in splitIntoBlocks' (accblocks<>acc2Para acc<>PB.bulletList (map f items)) [] rest 
splitIntoBlocks' accblocks acc (Right (SExp [Sym "tbl",SExp (Sym "list":firstrow:rows)]):rest) =
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.simpleTable (mkrow firstrow) (map mkrow rows)) [] rest
splitIntoBlocks' accblocks acc (Right (SExp [Sym "tbl",SExp (Sym "quote":firstrow:rows)]):rest) =
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.simpleTable (mkrow firstrow) (map mkrow rows)) [] rest
splitIntoBlocks' accblocks acc (Right (SExp (Sym "subpage":Int n:x:xs)):rest) = 
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.header (fromIntegral n) (sexp2Inlines x)) [] rest
splitIntoBlocks' accblocks acc (Right (SExp (Sym "section":x:xs)):rest) = 
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.header 1 (sexp2Inlines x)) [] rest
splitIntoBlocks' accblocks acc (Right (SExp (Sym "subsection":x:xs)):rest) = 
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.header 2 (sexp2Inlines x)) [] rest
splitIntoBlocks' accblocks acc (Right (SExp (Sym "subsubsection":x:xs)):rest) = 
  splitIntoBlocks' (accblocks<>acc2Para acc<>PB.header 3 (sexp2Inlines x)) [] rest
splitIntoBlocks' accblocks acc (Right s:rest) = splitIntoBlocks' accblocks (Right s:Left " ":acc) rest

splitIntoBlocks :: [Either T.Text SExp] -> PB.Blocks
splitIntoBlocks = splitIntoBlocks' mempty []

removeLeadingNewline :: [T.Text] -> [T.Text]
removeLeadingNewline [] = []
removeLeadingNewline (x:xs)
  | T.null $ T.strip x = xs
  | otherwise = x:xs
linesplitter :: [Either String SExp] -> [Either T.Text SExp]
linesplitter xs =
  let f (Left s) = map Left (removeLeadingNewline $ T.lines (T.pack s))
      f (Right s) = [ Right s ]
  in xs >>= f

scrbl2Pan :: [Either String SExp] -> PB.Pandoc
scrbl2Pan xs = 
  let blocks = splitIntoBlocks $ linesplitter xs in
  PB.doc blocks
