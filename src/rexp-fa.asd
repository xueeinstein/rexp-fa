;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage :com.shadowthink.rexpfa-system
  (:use :cl :asdf)
  (:nicknames :st-rexpfa-system))

(in-package :st-rexpfa-system)

(defsystem rexp-fa
  :name "rexp-fa"
  :author "Bill Xue <xy@shadowthink.com>"
  :version "0.1.0"
  :maintainer "Bill Xue <xy@shadowthink.com>"
  :license "MIT"
  :description "A cl tool to help converting regular expressions to finite automata"
  :components ((:file "packages")
               (:file "rexp-fa"
                      :depends-on ("packages")))
  :depends-on (:cl-ppcre))
