-include ../config/do.mk

DO_what=      xfun: semi-supervised multi-objective explanation (in LISP)
DO_copyright= Copyright (c) 2023 Tim Menzies, BSD-2.
DO_repos=     . ../config ../data

install: $(DO_repos)  ## get related repos
	brew install rlwrap clisp sbcl

../data:
		(cd ..; git clone https://gist.github.com/d47b8699d9953eef14d516d6e54e742e.git data)

../config:
		(cd ..; git clone https://gist.github.com/42f78b8beec9e98434b55438f9983ecc.git config)

