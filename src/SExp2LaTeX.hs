{-# LANGUAGE OverloadedStrings #-}

module SExp2LaTeX where

import           NaiveSExp
import qualified Data.Text as T
import           Text.LaTeX
import qualified Text.LaTeX.Packages.AMSMath as MATH
import qualified Text.LaTeX.Base.Commands as COMM
import qualified Text.LaTeX.Base.Syntax as SYNT

rawstr :: Monad m => String -> LaTeXT_ m
rawstr = raw . T.pack



isComment :: SExp -> Bool
isComment (Comment _) = True
isComment _ = False

rmComments :: SExp -> SExp
rmComments (SExp xs) = SExp $ map rmComments (filter (not . isComment) xs)
rmComments x = x

unStr :: SExp -> String
unStr (Str x) = x


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
sexp2LaTeX (Comment _) = mempty
sexp2LaTeX (SExp (Sym "use-LaTeX-preamble" : xs)) = do
  raw "\n%BystroTeX-preamble-start\n"
  sequence_ [ sexp2LaTeX x | x <- xs ]
  raw "\n%BystroTeX-preamble-end\n"
sexp2LaTeX (SExp [(Sym "void"), (Sym "BystroTeX-start-appendix")]) = COMM.appendix

sexp2LaTeX (SExp [Sym "cite", Str x]) = raw . T.pack $ "\\cite{" ++ x ++ "}"
sexp2LaTeX (SExp [Sym "seclink", Str x]) = rawstr "Section " >> (COMM.ref $ rawstr x)
sexp2LaTeX (SExp [Sym "verb", Str x]) = verbatim $ T.pack x
sexp2LaTeX (SExp [Sym "italic", Str x]) = textit $ rawstr x
sexp2LaTeX (SExp (Sym "bold": xs)) = emph $ sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "elem": xs)) = sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "itemlist" : Keyword "style" :  Sym "ordered" : xs)) = 
  COMM.enumerate $ sequence_ [ COMM.item Nothing >> sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "itemlist" : xs)) =
  COMM.itemize $ sequence_ [ COMM.item Nothing >> sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "item" : xs)) = sequence_ [ sexp2LaTeX x | x <- xs ]
sexp2LaTeX (SExp (Sym "comment" : xs)) = COMM.footnote $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp (Sym "larger" : xs)) = COMM.large  $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (SExp [Sym "hspace", Int n]) = COMM.hspace $ SYNT.Ex $ fromIntegral n
sexp2LaTeX (SExp (Sym "larger-2" : xs)) = COMM.large2  $ sequence_ [ sexp2LaTeX x | x <- xs]
sexp2LaTeX (Str a) = rawstr a
sexp2LaTeX (Int a) = rawstr $ show a
sexp2LaTeX (Dbl a) = rawstr $ show a
sexp2LaTeX (SExp (Sym "page": x: Keyword "tag": Str lbl: _)) = section (sexp2LaTeX x) >> label (rawstr lbl)
sexp2LaTeX (SExp (Sym "subpage": Int n: x: Keyword "tag": Str lbl: _)) = case n of
  1 -> subsection (sexp2LaTeX x) >> label (rawstr lbl)
  2 -> subsubsection (sexp2LaTeX x) >> label (rawstr lbl)
sexp2LaTeX (SExp [Sym "f", Str x]) = MATH.math $ rawstr x
sexp2LaTeX (SExp [Sym "v+", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "v-", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "h+", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "h-", i, f]) = sexp2LaTeX f
sexp2LaTeX (SExp [Sym "th-num", Str x]) = rawstr $ "\\refstepcounter{Theorems}\\label{" ++ x ++ "}\\noindent{\\bf \\arabic{Theorems}}"
sexp2LaTeX (SExp [Sym "th-ref", Str x]) = rawstr $ "\\ref{" ++ x ++ "}"
sexp2LaTeX (SExp [Sym "ref", Str x]) = ref $ rawstr x
sexp2LaTeX (SExp (Sym "e":xs)) = getEq Nothing [] xs
  where
  getEq Nothing    []    [] = error "ERROR: empty equation"
  getEq Nothing    mvals [] = MATH.equation (rawstr $ concat $ reverse mvals)
  getEq (Just lbl) mvals [] = MATH.equation $ (rawstr $ concat $ reverse mvals) >> label (rawstr lbl)
  getEq mlbl mvals (Keyword "label" : Str l : rest) = getEq (Just l) mvals rest
  getEq mlbl mvals (Str v:rexp) = getEq mlbl (v:mvals) rexp
sexp2LaTeX (SExp (Sym "align" : Sym "r.l.n" : xss)) = MATH.align (map f xss)
  where
  f (SExp [Sym "list", f1, f2, lbl ]) =
    let
      rendf (SExp [Sym "f", Str x]) = rawstr x
      rendf (SExp [Sym "f"]) = rawstr ""
      rendf (SExp (Sym "elem": Keyword "style" : Sym "no-break" : xs)) = mbox $ sexp2LaTeX (SExp (Sym "elem": xs))
      rendf (SExp (Sym "elem": x)) = mbox $ sexp2LaTeX (SExp (Sym "elem": x))
      rendf (SExp [Sym "v+", i, f]) = rendf f
      rendf (SExp [Sym "v-", i, f]) = rendf f
      rendf (SExp [Sym "h+", i, f]) = rendf f
      rendf (SExp [Sym "h-", i, f]) = rendf f
      -- TODO: add more here
      rendf (Str "") = rawstr " "
      rendf (Str x) = mbox $ rawstr x
      r = rendf f1 >> rawstr "\n &" >> rendf f2
    in
    case lbl of
      SExp [ Sym "label", Str l ] -> rawstr " " >> r >> label (rawstr l) >> rawstr " "
      Str "" -> rawstr " " >> r >> MATH.nonumber >> rawstr " "
      x -> rawstr $ show x
  f (SExp [Sym "quasiquote", f1, f2, lbl ]) = f (SExp [Sym "list", g f1, g f2, g lbl])
    where
    g (SExp [Sym "unquote", x]) = x
    g (SExp (Sym "unquote": rest)) = SExp rest
    g x = x
  f x = rawstr $ show x
sexp2LaTeX (SExp (Sym "align" : Sym "l.n" : xss)) =
  let insertEmpty (SExp (Sym "list": xs)) = SExp (Sym "list": Str "": xs)
      insertEmpty (SExp (Sym "quasiquote": xs)) = SExp (Sym "quasiquote": Str "": xs)
  in  sexp2LaTeX (SExp (Sym "align" : Sym "r.l.n" : map insertEmpty xss))
sexp2LaTeX (SExp [Sym "tt", Str x]) = texttt $ rawstr (show x)
sexp2LaTeX x = large2 . texttt $ rawstr ( show x )

piece2LaTeX :: Monad m => Either String SExp -> LaTeXT_ m
piece2LaTeX (Left x)  = rawstr x
piece2LaTeX (Right x) = sexp2LaTeX $ rmComments x

scrbl2LaTeX :: Monad m => [Either String SExp] -> LaTeXT_ m
scrbl2LaTeX = mapM_ piece2LaTeX






