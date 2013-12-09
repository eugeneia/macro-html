;;;; Packages for MACRO-HTML.

(defpackage macro-html
  (:documentation "DSL for generating HTML documents.")
  (:use :cl
	:named-readtables)
  (:shadow :map
           :time)
  (:export :html-doctype
	   :syntax))

(defpackage macro-html.widgets
  (:documentation "Predefined HTML widgets part of MACRO-HTML.")
  (:use :cl
        :macro-html
	:named-readtables)
  (:export :html-widget-head
	   :html-widget-meta
	   :html-widget-document
	   :html-widget-list
	   :html-widget-input
	   :html-widget-select
	   :html-widget-textarea
	   :html-widget-form
	   :html-widget-table)
  (:shadow :map
           :time))
