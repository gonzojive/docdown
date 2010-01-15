;;; Copyright (C) 2009 Leslie P. Polzer
;;; All rights reserved.
;;; See the file LICENSE for terms of use and distribution.

(in-package #:cl-user)

(defpackage :docdown-asd
  (:use #:cl #:asdf))

(in-package :docdown-asd)

(defsystem :docdown
  :name "Docdown"
  :description "Common Lisp OAuth implementation"
  :version "2"
  :maintainer "Red Daly <reddaly@gmail.com>"
  :licence "BSD"
  :components ((:static-file "docdown.asd")
               (:module "src"
                        :components ((:file "package"))))

  :depends-on (:alexandria :cl-markdown
               :closer-mop :contextl :metabang-bind
               :css-sexp :cl-who
               :puri))
