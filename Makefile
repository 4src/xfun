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


~/tmp/%.pdf: %.py  ## .lua ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-Br                \
		--file-align=fill      \
		--line-numbers=1        \
 		--lines-per-page=100 \
		--pro=color               \
		--left-footer="$<"            \
		--borders=no             \
		--columns 2                  \
		-M letter                     \
		--footer=""                    \
                --right-footer="%s. of %s#"               \
	  -o	 $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps
	open $@


~/tmp/%.pdf: %.lisp  ## .lua ==> .pdf
	mkdir -p ~/tmp
	echo "pdf-ing $@ ... "
	a2ps                 \
		-Br                \
		--file-align=fill      \
		--line-numbers=1        \
 --lines-per-page=100 \
		--pro=color               \
		--left-footer="$<"            \
		--borders=no             \
		--columns 2                  \
		-M letter                     \
		--footer=""                    \
                --right-footer="%s. of %s#"               \
	  -o	 $@.ps $<
	ps2pdf $@.ps $@; rm $@.ps
	open $@

.SILENT:
lispspaces:
	sudo add-apt-repository -y ppa:ubuntuhandbook1/emacs
	sudo apt update
	sudo apt install -y emacs emacs-common rlwrap  sbcl
	@echo "$$DOT_COM" > dot
	@echo "$$ISP" > isp1; chmod +x isp1
	printf "\n\n\n-----------------------\n"
	echo "got to via: emacs -l dot"
	echo "M-x package-install<RET>slime<RET>"
	echo call emacs using "emacs -l dot"

define DOT_COM
;-----------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq inferior-lisp-program "sbcl")
(global-set-key (kbd "C-M-(") 'mwheel-scroll)
(global-set-key (kbd "C-M-)") 'mwheel-scroll)
(setq column-number-mode t) 
(tool-bar-mode 0) 
(setq inhibit-startup-message t)
(setq make-backup-files nil) 
(global-font-lock-mode t)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(setq scroll-step 1)
(global-hl-line-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 52)
endef
export DOT_COM

define ISP
#!/usr/bin/env bash
f=$$1.lisp
shift
$$(which sbcl) --noinform --script $$f  $$* \
  2> >( gawk '/^Backtrace / {exit} 1' ) 
endef
export ISP
