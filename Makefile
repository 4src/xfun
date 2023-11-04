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

repl:
	rlwrap sbcl --noinform -
	
sbcl:;  sbcl --noinform --script $l $l 2> >( gawk '{print} /^Backtrace/ {exit}' )  ## run sbcl

install-codespaces:
	sudo apt -q update
	sudo apt -q upgrade
	sudo apt -q install rlwrap clisp sbcl