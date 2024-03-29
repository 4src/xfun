-include ../config/do.mk

saved: ## grab a commit message, commit github
	read -p "commit msg> " x; git commit -am "$$x"; git push; git status

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

sbcl:;  sbcl --noinform --script $l $l 2> >( gawk '{print} /^Backtrace/ {exit}' )  

install-codespaces:
	sudo apt -q update
	sudo apt -q upgrade
	sudo apt -q install rlwrap clisp sbcl

FILES=$(wildcard *.lisp)
docs: $(addprefix docs/,$(FILES:.lisp=.md))

docs/%.md : %.lisp
	@echo "$^ ==> $@"
	@gawk -f etc/lisp2md.awk -v file="$^" $^ > $@

BODY='BEGIN {RS=""; FS="\n"} NR==1 { next } { print($$0 "\n")  }'
HEAD='BEGIN {RS=""; FS="\n"} NR==1 { print($$0 "\n"); exit }'

%.md: $(which gawk) ## file.md  insert snips from code into markdown
	echo "# filling in $@ ..."
	gawk --source $(HEAD) README.md >  _in
	gawk --source $(BODY) $@                  >> _in
	gawk -f etc/snips.awk PASS=1 *.lisp  PASS=2 _in > _out
	mv _out $@; rm _in