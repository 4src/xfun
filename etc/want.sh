#!/usr/bin/env bash
gawk 'BEGIN {RS=";;;"} gsub(" "want,"",$0) {print $0}' want="$1" core.lisp
