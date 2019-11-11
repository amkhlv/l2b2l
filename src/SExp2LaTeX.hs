{-# LANGUAGE OverloadedStrings #-}

module SExp2LaTeX where

import           NaiveSExp
import qualified Data.Text as T
import           Text.LaTeX
import qualified Text.LaTeX.Packages.AMSMath as MATH

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

ignore :: Monad m => LaTeXT_ m
ignore = rawstr ""

sexp2LaTeX :: Monad m => SExp -> LaTeXT_ m
sexp2LaTeX (SExp (Sym "require": _)) = ignore
sexp2LaTeX (SExp (Sym "bystro-set-css-dir": _)) = ignore
sexp2LaTeX (SExp (Sym "define": _)) = ignore
sexp2LaTeX (SExp (Sym "bystro-def-formula": _)) = ignore
sexp2LaTeX (SExp [Sym "bystro-toc"]) = ignore
sexp2LaTeX (SExp (Sym "bystro-close-connection": _)) = ignore
sexp2LaTeX (SExp (Sym "disconnect": _)) = ignore
sexp2LaTeX (SExp (Sym "title": rest)) = ignore
sexp2LaTeX (Comment _) = ignore

sexp2LaTeX (SExp [Sym "verb", Str x]) = verbatim $ T.pack x
sexp2LaTeX (SExp [Sym "bold", Str x]) = emph $ rawstr x
sexp2LaTeX (SExp (Sym "elem": xs)) = sequence_ [ sexp2LaTeX x | x <- xs ]
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
sexp2LaTeX (SExp [Sym "ref", Str x]) = MATH.eqref $ rawstr x
sexp2LaTeX (SExp (Sym "e":xs)) = getEq Nothing [] xs
  where
  getEq Nothing    []    [] = error "ERROR: empty equation"
  getEq Nothing    mvals [] = MATH.equation (rawstr $ concat $ reverse mvals)
  getEq (Just lbl) mvals [] = MATH.equation (rawstr $ concat $ reverse mvals) >> label (rawstr lbl)
  getEq mlbl mvals (Keyword "label" : Str l : rest) = getEq (Just l) mvals rest
  getEq mlbl mvals (Str v:rexp) = getEq mlbl (v:mvals) rexp
sexp2LaTeX (SExp (Sym "align" : Sym "r.l.n" : xss)) = MATH.align (map f xss)
  where
  f (SExp [Sym "list", f1, f2, lbl ]) =
    let
      rendf (SExp [Sym "f", Str x]) = rawstr x
      rendf (SExp [Sym "f"]) = rawstr ""
      rendf (SExp (Sym "elem": x)) = mbox $ sexp2LaTeX (SExp (Sym "elem": x))
      rendf (SExp [Sym "v+", i, f]) = rendf f
      rendf (SExp [Sym "v-", i, f]) = rendf f
      rendf (SExp [Sym "h+", i, f]) = rendf f
      rendf (SExp [Sym "h-", i, f]) = rendf f
      -- TODO: add more here
      rendf (Str x) = mbox $ rawstr x
      r = rendf f1 >> rawstr ("\n &") >> rendf f2
    in
    case lbl of
      SExp [ Sym "label", Str l ] -> r >> label (rawstr l)
      Str "" -> r
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
sexp2LaTeX x = large2 . texttt $ rawstr ( show x )

piece2LaTeX :: Monad m => Either String SExp -> LaTeXT_ m
piece2LaTeX (Left x)  = rawstr x
piece2LaTeX (Right x) = sexp2LaTeX $ rmComments x

scrbl2LaTeX :: Monad m => [Either String SExp] -> LaTeXT_ m
scrbl2LaTeX = mapM_ piece2LaTeX






