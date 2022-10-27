#!/usr/bin/sbcl --script

(load "~/.quicklisp/setup.lisp")

(ql:quickload :l-shell :silent t)

(in-package :l-shell)

(repl)


