{-# LANGUAGE OverloadedStrings #-}

module SExp2LaTeX where

import           NaiveSExp
import qualified Data.Text as T
import qualified Data.List as L
import           Text.LaTeX
import qualified Text.LaTeX.Packages.AMSMath as MATH
import qualified Text.LaTeX.Base.Commands as COMM
import qualified Text.LaTeX.Base.Syntax as SYNT
import qualified Text.LaTeX.Base.Types as TYPE
import qualified Text.LaTeX.Packages.Hyperref as HREF
import qualified Text.LaTeX.Packages.Color as COLOR
import           Text.LaTeX.Packages.Graphicx
import qualified Data.Maybe as MB

rawstr :: Monad m => String -> LaTeXT_ m
rawstr = raw . T.pack


rawstrtrim :: Monad m => String -> LaTeXT_ m
rawstrtrim = raw . T.strip . T.pack


isComment :: SExp -> Bool
isComment (Comment _) = True
isComment _ = False

rmComments :: SExp -> SExp
rmComments (SExp xs) = SExp $ map rmComments (filter (not . isComment) xs)
rmComments x = x

unStr :: SExp -> String
unStr (Str x) = x

unQuote (SExp [Sym "unquote", x]) = x
unQuote (SExp (Sym "unquote": rest)) = SExp rest
unQuote x = x

mkRow' :: Monad m => LaTeXT_ m -> [SExp] -> LaTeXT_ m
mkRow' acc [] = acc
mkRow' acc (x:xs) = mkRow' (acc COMM.& sexp2LaTeX x) xs
mkRow  :: Monad m => [SExp] -> LaTeXT_ m
mkRow (x:xs) = mkRow' (sexp2LaTeX x) xs

imgext :: String -> String
imgext x
  | take 4 (reverse x) == "gvs." = reverse ( drop 3 $ reverse x ) ++ "png"
  | otherwise = x

rendf (SExp [Sym "f", Str x]) = rawstrtrim  x
rendf (SExp [Sym "f"]) = rawstr ""
-- rendf (Sym "f") = rawstr ""
rendf (SExp (Sym "elem": Keyword "style" : Sym "no-break" : xs)) = mbox $ sexp2LaTeX (SExp (Sym "elem": xs))
rendf (SExp (Sym "elem": x)) = mbox $ sexp2LaTeX (SExp (Sym "elem": x))
rendf (SExp [Sym "v+", i, f]) = rendf f
rendf (SExp [Sym "v-", i, f]) = rendf f
rendf (SExp [Sym "h+", i, f]) = rendf f
rendf (SExp [Sym "h-", i, f]) = rendf f
-- TODO: add more here
rendf (Str "") = rawstr " "
rendf (Str x) = mbox $ rawstr x
rendf x = error $ "Unknown pattern in align[]:" ++ show x
sexp2LaTeX :: Monad m => SExp -> LaTeXT_ m
sexp2LaTeX (SExp (Sym "require": _)) = mempty
sexp2LaTeX (SExp (Sym "bystro-set-css-dir": _)) = mempty
sexp2LaTeX (SExp (Sym "define": _)) = mempty
sexp2LaTeX (SExp (Sym "bystro-def-formula": _)) = mempty
sexp2LaTeX (SExp [Sym "bystro-toc"]) = mempty
sexp2LaTeX (SExp [Sym "bystro-local-toc"]) = mempty
sexp2LaTeX (SExp (Sym "bystro-close-connection": _)) = mempty
sexp2LaTeX (SExp [Sym "bystro-source"]) = mempty
sexp2LaTeX (SExp [Sym "bystro-ribbon"]) = mempty
sexp2LaTeX (SExp (Sym "disconnect": _)) = mempty
sexp2LaTeX (SExp (Sym "title": rest)) = mempty
sexp2LaTeX (SExp [Sym "bibliography"]) = mempty
sexp2LaTeX (SExp [Sym "fsize="]) = mempty
sexp2LaTeX (SExp (Sym "fsize+" : _)) = mempty
sexp2LaTeX (SExp (Sym "elemtag" : _)) = mempty
sexp2LaTeX (SExp (Sym "bystro-margin-note": _)) = mempty
sexp2LaTeX (Comment _) = mempty
sexp2LaTeX (SExp (Sym "bystro-abstract": rest)) = mempty
-- sexp2LaTeX (SExp (Sym "bystro-abstract": rest)) = COMM.abstract $ sequence_ [sexp2LaTeX x | x <- rest]
sexp2LaTeX (SExp (Sym "bystro-authors": rest)) = mempty
sexp2LaTeX (SExp [Sym "table-of-contents"]) = mempty
sexp2LaTeX (SExp (Sym "autolist-pdfs": rest)) = mempty
sexp2LaTeX (SExp (Sym "autolist-svgs": rest)) = mempty
sexp2LaTeX (SExp (Sym "autolist-images": rest)) = mempty
sexp2LaTeX (SExp (Sym "tg": Sym "talk": rest)) = mempty
sexp2LaTeX (SExp [Sym "high"]) = mempty
sexp2LaTeX (SExp [Sym "bystro-reset-colors"]) = mempty
sexp2LaTeX (SExp (Sym "bystro-scrbl-only" : xs)) = mempty
sexp2LaTeX (SExp (Sym "bystro-latex-only" : Str x : xs)) = rawstr x >> sexp2LaTeX (SExp (Sym "bystro-latex-only" : xs))
sexp2LaTeX (Sym "appendix") = COMM.appendix
sexp2LaTeX (SExp (Sym "use-LaTeX-preamble" : xs)) = do
  raw "\n%BystroTeX-preamble-start\n"
  sequence_ [ sexp2LaTeX x | x <- xs ]
  raw "\n%BystroTeX-preamble-end\n"
sexp2LaTeX (SExp [(Sym "void"), (Sym "BystroTeX-start-appendix")]) = COMM.appendix

sexp2LaTeX (SExp (Sym "indent": xs)) = sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "indent--->": xs)) = sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp [Sym "cite", Str x]) = raw . T.pack $ "\\cite{" ++ x ++ "}"
sexp2LaTeX (SExp [Sym "seclink", Str x]) = rawstr "Section " >> (COMM.ref $ rawstr x)
sexp2LaTeX (SExp (Sym "seclink": (Str x : _))) = sexp2LaTeX (SExp [Sym "seclink", Str x])
sexp2LaTeX (SExp [Sym "verb", Str x]) = verbatim $ T.pack x
sexp2LaTeX (SExp [Sym "italic", Str x]) = textit $ rawstr x
sexp2LaTeX (SExp (Sym "bold": xs)) = emph $ sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "emph": xs)) = emph $ sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "elem": xs)) = sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "itemlist" : Keyword "style" :  Sym "ordered" : xs)) =
  COMM.enumerate $ sequence_ [ COMM.item Nothing >> sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "itemlist" : xs)) =
  COMM.itemize $ sequence_ [ COMM.item Nothing >> sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "item" : xs)) = sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "comment" : xs)) = COMM.footnote $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "summary" : xs)) = sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "larger" : xs)) = COMM.large  $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "larger-2" : xs)) = COMM.large2  $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp [Sym "hspace", Int n]) = COMM.hspace $ SYNT.Ex $ fromIntegral n
sexp2LaTeX (SExp [Sym "hrule"]) = COMM.hrulefill
sexp2LaTeX (Sym "noindent") = COMM.noindent
sexp2LaTeX (SExp [Sym "linebreak"]) = rawstr "\n\n\\vspace{10pt}\n"
sexp2LaTeX (Str a) = rawstr a
sexp2LaTeX (Int a) = rawstr $ show a
sexp2LaTeX (Dbl a) = rawstr $ show a
sexp2LaTeX (SExp (Sym "page": x: Keyword "tag": Str lbl: _)) = section (sexp2LaTeX x) >> label (rawstr lbl)
sexp2LaTeX (SExp (Sym "subpage": Int n: x: Keyword "tag": Str lbl: _)) = case n of
  1 -> subsection (sexp2LaTeX x) >> label (rawstr lbl)
  2 -> subsubsection (sexp2LaTeX x) >> label (rawstr lbl)
  3 -> paragraph (sexp2LaTeX x) >> label (rawstr lbl)
sexp2LaTeX (SExp (Sym "section": Keyword "tag" : Str tg : xs)) = section (sequence_ [ sexp2LaTeX x | x <- xs]) >> label (rawstr tg)
sexp2LaTeX (SExp (Sym "subsection": Keyword "tag" : Str tg : xs)) = subsection (sequence_ [ sexp2LaTeX x | x <- xs]) >> label (rawstr tg)
sexp2LaTeX (SExp (Sym "subsubsection": Keyword "tag" : Str tg : xs)) = subsubsection (sequence_ [ sexp2LaTeX x | x <- xs]) >> label (rawstr tg)
sexp2LaTeX (SExp (Sym "section": xs)) = section $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "subsection" : xs)) = subsection $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "subsubsection" : xs)) = subsubsection $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp [Sym "f", Str x]) = MATH.math $ rawstr x
sexp2LaTeX (SExp [Sym "v+", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "v-", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "h+", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "h-", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "th-num", Str x]) = rawstr $ "\\refstepcounter{Theorems}\\label{" ++ x ++ "}\\noindent{\\bf \\arabic{Theorems}}"
sexp2LaTeX (SExp [Sym "th-ref", Str x]) = rawstr $ "\\ref{" ++ x ++ "}"
sexp2LaTeX (SExp [Sym "defn-num", Str x]) = rawstr $ "\\refstepcounter{Definitions}\\label{" ++ x ++ "}\\noindent{\\bf \\arabic{Definitions}}"
sexp2LaTeX (SExp [Sym "defn-ref", Str x]) = rawstr $ "\\ref{" ++ x ++ "}"
sexp2LaTeX (SExp [Sym "spn", Sym "attn", Str x]) = textbf $ rawstr x
sexp2LaTeX (SExp [Sym "ref", Str x]) = ref $ rawstr x
sexp2LaTeX (SExp (Sym "div": Sym _: xs)) = sexp2LaTeX (SExp (Sym "bold": xs))
sexp2LaTeX (SExp [Sym "image", Str x]) = includegraphics [] $ imgext x
sexp2LaTeX (SExp [Sym "image", Keyword "scale", Dbl f, Str x]) = includegraphics [ IGScale $ realToFrac f ] $ imgext x
sexp2LaTeX (SExp [Sym "image", Keyword "scale", Int f, Str x]) = includegraphics [ IGScale $ fromIntegral f ] $ imgext x
sexp2LaTeX (SExp (Sym "e":xs)) = getEq Nothing [] xs
  where
  getEq Nothing    []    [] = error "ERROR: empty equation"
  getEq Nothing    mvals [] = MATH.equation (rawstr $ concat $ reverse mvals)
  getEq (Just lbl) mvals [] = MATH.equation $ (rawstr $ concat $ reverse mvals) >> label (rawstr lbl)
  getEq mlbl mvals (Keyword "label" : Str l : rest) = getEq (Just l) mvals rest
  getEq mlbl mvals (Str v:rexp) = getEq mlbl (v:mvals) rexp
sexp2LaTeX (SExp (Sym "align" : Sym "l.n" : xss)) =
  let insertEmpty (SExp (Sym "list": xs)) = SExp (Sym "list": Str "": xs)
      insertEmpty (SExp (Sym "quasiquote": xs)) = SExp (Sym "quasiquote": Str "": xs)
  in  sexp2LaTeX (SExp (Sym "align" : Sym "r.l.n" : map insertEmpty xss))
sexp2LaTeX (SExp (Sym "align" : Sym "r.l.n" : xss)) = MATH.align (map f xss >>= MB.maybeToList)
  where
  f (SExp (Sym "bystro-scrbl-only" : rest)) = Nothing
  f (SExp (Sym "quasiquote": xs)) = f (SExp (Sym "list": map g xs))
    where
    g (SExp [Sym "unquote", x]) = x
    g (SExp (Sym "unquote": rest)) = SExp rest
    g x = x
  f (SExp [Sym "list", f1, f2, lbl]) = case lbl of
    SExp [ Sym "label", Str l ] -> Just $
      rawstr " " >> rendf f1 >> rawstr "\n & " >> rendf f2 >> rawstr "\n " >> label (rawstr l) >> rawstr " "
    Str "" -> Just $
      rawstr " " >> rendf f1 >> rawstr "\n & " >> rendf f2 >> rawstr "\n " >> MATH.nonumber >> rawstr " "
    x -> Just (rawstr $ show x)
  f x = error $ "ERROR: line does not fit r.l.n pattern: " ++ show x
sexp2LaTeX (SExp (Sym "align" : sym : xss)) = MATH.align (map f xss >>= MB.maybeToList)
  where
  f (SExp (Sym "bystro-scrbl-only" : rest)) = Nothing
  f (SExp ( Sym "list": rest)) =
    let
      r = sequence $ L.intersperse (rawstr " ") (map rendf (init rest))
      lbl = last rest
    in
    case lbl of
      SExp [ Sym "label", Str l ] -> Just $ rawstr " &" >> r >> label (rawstr l) >> rawstr " "
      Str "" -> Just $ rawstr " &" >> r >> MATH.nonumber >> rawstr " "
      x -> Just (rawstr $ show x)
  f (SExp (Sym "quasiquote": xs)) = f (SExp (Sym "list": map g xs))
    where
    g (SExp [Sym "unquote", x]) = x
    g (SExp (Sym "unquote": rest)) = SExp rest
    g x = x
  f x = Just (rawstr $ show x)
sexp2LaTeX (SExp [Sym "tt", Str x]) = texttt $ rawstr (show x)
sexp2LaTeX (SExp [Sym "tbl", Keyword "orient", o, SExp (Sym "quasiquote" : xs)]) =
  let mkTable zs = case zs of
        [] -> COMM.lnbk >> COMM.hline
        SExp ws : rest -> COMM.lnbk >> COMM.hline >> rawstr " " >> mkRow [ unQuote w | w <- ws ] >> mkTable rest
        o -> large2 . texttt $ rawstr ( show o )
  in case xs of
       SExp ys : rest ->
         COMM.tabular
         Nothing
         ( TYPE.VerticalLine : L.intersperse TYPE.VerticalLine [ TYPE.LeftColumn | _ <- ys ] ++ [TYPE.VerticalLine] )
         ( COMM.hline >> mkRow [ unQuote y | y <- ys ] >> mkTable rest )
sexp2LaTeX (SExp (Sym "hyperlink" : Str h : xs)) =
    HREF.href [] (HREF.createURL h) (COMM.textbf $ COLOR.textcolor (COLOR.DefColor COLOR.Blue) (sequence_ [sexp2LaTeX x | x <- xs]))
sexp2LaTeX (SExp (Sym "spn": Sym "attn": rest)) = COMM.textbf $ COLOR.textcolor (COLOR.DefColor COLOR.Red) (sequence_ [sexp2LaTeX x | x <- rest])
-- FALLBACK:
sexp2LaTeX x = large2 . texttt $ rawstr ( show x )


piece2LaTeX :: Monad m => Either String SExp -> LaTeXT_ m
piece2LaTeX (Left x)  = rawstr x
piece2LaTeX (Right x) = sexp2LaTeX $ rmComments x

scrbl2LaTeX :: Monad m => [Either String SExp] -> LaTeXT_ m
scrbl2LaTeX = mapM_ piece2LaTeX

