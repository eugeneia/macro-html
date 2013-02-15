;;;; Define named readtable for attribute syntax.

(in-package :html)

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\[ #'read-attribute-set)
  (:macro-char #\] (get-macro-character #\)))
  (:case :invert))
