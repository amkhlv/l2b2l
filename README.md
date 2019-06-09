Synopsis
========

This project provides tools for automatic conversion between `LaTeX` 
and [BystroTeX](http://andreimikhailov.com/slides/bystroTeX/slides-manual/index.html)


Installation
============

    sudo apt-get install stack

Then `cd` to __this__ directory and say:

    stack install

It will install the binaries `l2b` and `b2l` into `~/.local/bin/`

Use
===

    cat myTeXFile.tex | l2b

This will print the `BystroTeX` on `stdout`

    cat example.scrbl | tail -n +2 | b2l -f > example.tex


Limitations
===========

The converter `LaTeX → BystroTeX` is very naive, it only works on a small subset of `LaTeX` which I know.
Everybody writes `LaTeX` in their own way. 
The program can be customized by editing the source code `src/L2B.hs`.

The converter `BystroTeX → LaTeX` is still work in progress. (But it 
does work on `example.scrbl`)


