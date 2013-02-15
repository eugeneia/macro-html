;;;; Various widget functions for common HTML template pieces.

(defpackage html.widgets
  (:use :cl
	:named-readtables
        :html)
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

(in-package :html.widgets)
(in-readtable html:syntax)

(defparameter *default-encoding* :utf-8
  "Default document encoding.")

(defun html-widget-meta (&key (content-type "text/html")
			 (encoding *default-encoding*))
  "Print meta tag. Takes strings for CONTENT-TYPE and ENCODING."
  (meta :http-equiv "Content-Type"
	:content (format nil "~a; charset=~a"
			 content-type (symbol-name encoding))))

(defun html-widget-head (title &key stylesheets scripts feeds
			 (encoding *default-encoding*))
  "Print document head. Takes a string as TITLE and a symbol as ENCODING.
STYLESHEETS have to be a list of plists containing :href and :media
strings. SCRIPTS has to be a list of url strings. FEEDS has to be a list
of plists containing :type :title and :href strings."
  (head
   (html-widget-meta :encoding encoding)
   (when title
     (title title))
   (dolist (stylesheet stylesheets)
     (link :rel "stylesheet"
	   :type "text/css"
	   :href (getf stylesheet :href)
	   :media (getf stylesheet :media)))
   (dolist (script scripts)
     (script [:type "text/javascript" :src script]))
   (dolist (feed feeds)
     (link :rel "alternate"
	   :title (getf feed :title)
	   :type (getf feed :type)
	   :href (getf feed :href)))))

(defun html-widget-document (title body &key stylesheets scripts feeds
			     (encoding *default-encoding*))
  "Print html document. Takes TITLE, STYLESHEETS, SCRIPTS, FEEDS and
ENCODING like html-widget-head. BODY has to be a function that prints
the documents contents to *standard-output*."
  (html-doctype)
  (html
   (html-widget-head title
		     :stylesheets stylesheets
		     :scripts scripts
		     :encoding encoding
		     :feeds feeds)
   (body
    (funcall body))))

(defun html-widget-list (list &key (type :unordered)
			 (to-string (lambda (v) v)))
  "Print list. Takes a LIST of strings, a TYPE selector which can either
be :unordered (default), :ordered, or :definitions and a function
TO-STRING which will be applied to the strings and should return a string."
  (case type
    (:unordered
     (ul
      (dolist (item list)
	(li (funcall to-string item)))))
    (:ordered
     (ol
      (dolist (item list)
	(li (funcall to-string item)))))
    (:definitions
     (dl
      (loop for (item description) in list
	 do
	   (dt (funcall to-string :title item))
	   (dd (funcall to-string :description description)))))
    (otherwise
     (error "List type unimplemented."))))

(defun html-widget-input (name label type)
  "Print input tag with label. Takes three strings NAME, LABEL and TYPE."
  (label label)
  (br)
  (input :name name :type type))

(defun option-fields (option)
  "Returns value, label and selected of option."
  (values (first option) (second option) (eq :selected (third option))))

(defun option-group-p (list)
  "Returns nil unless LIST is an option group."
  (eq (first list) :group))

(defun html-select-options (options)
  "Helper for HTML-WIDGET-SELECT."
  (labels ((select-options (options top-level)
	     (loop for option in options
		do
		  (if (and top-level (option-group-p option))
		      (optgroup [:label (second option)]
				(select-options (rest (rest option)) nil))
		      (multiple-value-bind (value label selected)
			  (option-fields option)
			(if selected
			    (option :value value
				    :label label
				    :selected nil)
			    (option :value value
				    :label label)))))))
    (select-options options t)))

(defun html-widget-select (name label options &key multiple)
  "Print select tag with options. Takes two strings as NAME and LABEL.
OPTIONS has to be a list of options and option groups. Options are lists
in the form (<value-string> <label-string> [:selected]) and option groups
are lists staring with :group followed by options. If MULTIPLE is not nil
then multiple selections are allowed."
  (label label)
  (br)
  (if multiple
      (select [:name name :multiple nil]
	      (html-select-options options))
      (select [:name name]
	      (html-select-options options))))

(defun html-widget-textarea (name label initial-text)
  "Print textarea tag. Takes three strings NAME, LABEL, and
INITIAL-TEXT."
  (label label)
  (br)
  (textarea [:name name] initial-text))

(defun html-widget-form (action fields  &key (method "GET")
			 (description "Submit"))
  "Print form. Takes three strings ACTION, METHOD (defaults to \"GET\")
and DESCRIPTION (defaults to \"Submit\"). The function FIELDS should
print the form fields to *standard-output*."
  (form [:action action :method method]
	(p
	 (funcall fields)
	 (br)
	 (input :type "submit" :value description )
	 (input :type "reset"))))

(defun html-widget-table (head body)
  "Print table. Takes a row HEAD and a list of rows BODY. A row has to be
a list of strings."
  (table
   (thead
    (tr (loop for column in head do
	     (th column))))
   (tbody (loop for row in body do
	       (tr (loop for column in row do
			(td column)))))))
