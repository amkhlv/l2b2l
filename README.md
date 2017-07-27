Synopsis
========

This project provides tools for automatic conversion between `LaTeX` 
and [BystroTeX]("http://andreimikhailov.com/slides/bystroTeX/slides-manual/index.html")


Installation
============

    sudo apt-get install stack

Then `cd` to __this__ directory and say:

    stack install

It should install the binary `l2b` into `~/.local/bin/`

Use
===

    cat myTeXFile.tex | l2b

This will print the `BystroTeX` on `stdout`

Limitations
===========

At the moment, I only have `LaTeX → BystroTeX`.
The converter is very naive, it only works on a small subset of `LaTeX` which I know.
Everybody writes `LaTeX` in their own way. 
The program can be customized by editing the source code `src/L2B.hs`.

