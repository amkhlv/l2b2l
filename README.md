Synopsis
========

This project provides tools for automatic conversion between `LaTeX` 
and [BystroTeX](http://andreimikhailov.com/slides/bystroTeX/slides-manual/index.html)


Installation
============

Plain Cabal
-----------

    cabal install

Nix
---

The file `project.nix` is obtained by running:

    cabal2nix . > project.nix

Then, build:

    nix-build --no-sandbox


Use
===

LaTeX to BystroTeX
------------------

    cat myTeXFile.tex | l2b

This will print the `BystroTeX` on `stdout`

BystroTeX to LaTeX
------------------

    cat example.scrbl | tail -n +2 | b2l -f > example.tex

Limitations
===========

The converter `LaTeX → BystroTeX` is very naive, it only works on a small subset of `LaTeX` which I know.
Everybody writes `LaTeX` in their own way. 
The program can be customized by editing the source code `src/L2B.hs`.

The converter `BystroTeX → LaTeX` is still work in progress. (But it 
does work on `example.scrbl`)


