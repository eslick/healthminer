;;; -*- Mode: Lisp; Package: User; -*-

#|
  This is a hand-built Lisp interface to the Link Grammar Parser,
  following its API declarations in link-includes.h.  That parser is
  available from
  http://www.abisource.org/projects/link-grammar/
  and its API is described in
  http://www.abisource.com/projects/link-grammar/api/index.html
  as of March, 2009.

  That API supports five basic data structures: Dictionary, Sentence,
  Parse_Options, Linkage and PostProcessor.  In order to avoid memory
  leaks in creating these (and some other things) within Lisp, the
  appropriate (delete_...) calls should be made. We have tried to
  package these up in macros of the form (with-...), but for heavy
  weight items such as Dictionary, it may be more appropriate to
  create the structure and only delete it when it is no longer of
  use.  Note that only the five basic data structures and large
  strings returned by the linkage-print-... functions need to be
  deleted. The link parser appears to take care of managing memory for
  its short strings, such as individual tokens. In this interface, it
  is the functions that return :foreign-address that require special
  attention to potential memory leaks.

  An alternative to this implementation would be to wrap every
  link-parser object in a CLOS object and use the Lisp garbage
  collector's finalization mechanism to make sure that the link-parser
  object is deleted when the corresponding Lisp object is gc'd.  This
  would be more elegant and error-proof, but the with-... macros
  provide the same protection locally in code, with less overhead.

  This code was written in 2009 by Peter Szolovits (psz@mit.edu), and
  is made available under the "MIT License":

  Copyright (c) 2009 Peter Szolovits and Massachusetts Institute of
  Technology.  
  
  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation files
  (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions: 

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  |#

(in-package :smart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionary
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-foreign-library link-grammar
;;  (:darwin (:or "liblink-grammar.4.dylib" "liblink-grammar.dylib"))
  (t (:default "liblink-grammar")))
  

(cffi:defcfun "dictionary_create" :pointer
  (dict :string+ptr)
  (knowledge :string+ptr)
  (post :string+ptr)
  (affix :string+ptr))


(defun dict-create (dictname knowlname postname affixname)
  (with-foreign-string (c_dn (or dictname ""))
    (with-foreign-string (c_kn (or knowlname ""))
      (with-foreign-string (c_pn (or postname ""))
	(with-foreign-string (c_an (or affixname ""))
	  (format t "wtf?~%")
	  (dictionary-create 
	   (if dictname c_dn 0)
	   (if knowlname c_kn 0)
	   (if postname c_pn 0)
	   (if affixname c_an 0)))))))

(cffi:defcfun "dictionary_create_lang" :pointer
  (lang :string))


(cffi:defcfun "dictionary_create_default_lang" :pointer)

(cffi:defcfun "dictionary_delete" :int
  (dict :pointer))

;;; with dictionary, execute the following program (in body b).
(defmacro with-dictionary ((name &optional d k p a) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name (dict-create ,d ,k ,p ,a)))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (dictionary-delete ,name)))))

(defmacro with-dictionary-lang
    ((name &optional (language nil lang-given)) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name
		      ,(if lang-given
			   `(dictionary-create-lang ,language)
			 '(dictionary-create-default-lang))))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (dictionary-delete ,name)))))

(cffi:defcfun "dictionary_get_max_cost" :int
  (dict :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse_options
;;;
;;;
;;; Some of these options are numerical, others Boolean. We follow C
;;; conventions and require that the Boolean ones are set to 0 or 1 for
;;; FALSE or TRUE. Alternatively, we could declare these parameters to
;;; be of the form (argname :int boolean).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "parse_options_create" :pointer)

(cffi:defcfun "parse_options_delete" :int
  (po :pointer))

;;; Macro to allow use of a parse-options, making sure to delete it
;;; when done.  The value of the last form in the body is
;;; returned. Options, if given, is a list of alternating keywords and
;;; values, appropriate for set-parse-options.
(defmacro with-parse-options ((name &optional options) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (if (zerop (setq ,name (parse-options-create)))
	     (error "Unable to create parse_options.")
	   (progn
	     ,@(if options
		   (cons `(apply #'set-parse-options ,name ,options)
			 b)
		 b)))
       (when (and ,name (not (zerop ,name)))
	 (parse-options-delete ,name)))))

(cffi:defcfun "parse_options_set_verbosity" :void
  (po :pointer)
  (param :int fixnum))

(cffi:defcfun "parse_options_get_verbosity" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_linkage_limit" :void
  (po :pointer)
  (param :int))

(cffi:defcfun "parse_options_get_linkage_limit" :int
  (po :pointer))

(cffi:defcfun "parse_options_set_disjunct_cost" :void
  (po :pointer)
  (param :int fixnum))

(cffi:defcfun "parse_options_get_disjunct_cost" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_min_null_count" :void
  (po :pointer) 
  (param :int))


(cffi:defcfun "parse_options_get_min_null_count" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_max_null_count" :void
  (po :pointer) 
  (param :int))

(cffi:defcfun "parse_options_get_max_null_count" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_null_block" :void
  (po :pointer)
  (param :int fixnum))
 
(cffi:defcfun "parse_options_get_null_block" :int
  (po :pointer))

(cffi:defcfun "parse_options_set_islands_ok" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_islands_ok" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_short_length" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_short_length" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_max_memory" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_max_memory" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_max_sentence_length" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_max_sentence_length" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_max_parse_time" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_max_parse_time" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_cost_model_type" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_cost_model_type" :int
  (po :pointer))


(cffi:defcfun "parse_options_timer_expired" :int
  (po :pointer))


(cffi:defcfun "parse_options_memory_exhausted" :int
  (po :pointer))


(cffi:defcfun "parse_options_resources_exhausted" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_screen_width" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_screen_width" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_allow_null" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_allow_null" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_walls" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_walls" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_all_short_connectors" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_all_short_connectors" :int
  (po :pointer))


(cffi:defcfun "parse_options_reset_resources" :void
  (po :pointer))


(cffi:defcfun "parse_options_set_batch_mode" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_batch_mode" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_panic_mode" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_panic_mode" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_on" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_on" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_postscript" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_postscript" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_constituents" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_constituents" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_bad" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_bad" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_links" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_links" :int
  (po :pointer))


(cffi:defcfun "parse_options_set_display_union" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_union" :int
  (po :pointer))



(cffi:defcfun "parse_options_set_display_disjuncts" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_disjuncts" :int
  (po :pointer))



(cffi:defcfun "parse_options_set_display_senses" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_display_senses" :int
  (po :pointer))



(cffi:defcfun "parse_options_set_echo_on" :void
  (po :pointer)
  (param :int fixnum))


(cffi:defcfun "parse_options_get_echo_on" :int
  (po :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sentence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is a hugely incomplete whack at trying to define a Lisp
;;; version of the Sentence structure.  If I were better at FFI, I
;;; would do this for all the public structures of LG, making them
;;; more easily manipulable in Lisp.
(cffi:def-foreign-type Sentence
    (:struct (dict (* Dictionary))
	     (word (:array (* char)))
	     (is_conjunction (* char))
	     (deletable (* (* char)))
	     (dptr (* (* char)))
	     (effective_dist (* (* char)))
	     (num_linkages_found :int)
	     (num_linkages_allocated :int)
	     (num_linkages_post_processed :int)
	     (num_valid_linkages :int)
	     (null_links :int)
	     (null_count :int)
	     ;; There is actually more, but ...
	     ))

(cffi:defcfun "sentence_create" :pointer
  (sent :string)
  (dict :pointer))

(cffi:defcfun "sentence_delete" :void
  (sent :pointer))

(defmacro with-sentence ((name val dict) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name (sentence_create ,val ,dict)))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (sentence_delete ,name)))))

(cffi:defcfun "sentence_parse"
    ((sent :pointer)
     (opts :pointer))
  :strings-convert nil
  :returning :int)

(cffi:defcfun "sentence_length ((sent :pointer))"
  :returning :int)

(cffi:defcfun "sentence_get_word ((sent :pointer)"
					(wordnum :int))
  :returning ((* :char)))

(cffi:defcfun "sentence_null_count ((sent :pointer))"
  :returning :int)

(cffi:defcfun "sentence_num_linkages_found ((sent :pointer))"
  :returning :int)

(cffi:defcfun "sentence_num_valid_linkages ((sent :pointer))"
  :returning :int)

(cffi:defcfun "sentence_num_linkages_post_processed"
    ((sent :pointer))
  :returning :int)

(cffi:defcfun "sentence_num_violations "
    ((sent :pointer)
     (i :int))
  :returning :int)

;; This is in the API, but not the documentation.
(cffi:defcfun "sentence_and_cost"
    ((sent :pointer)
     (i :int))
  :returning :int)

(cffi:defcfun "sentence_disjunct_cost"
    ((sent :pointer)
     (i :int))
  :returning :int)

(cffi:defcfun "sentence_get_nth_word"
    ((sent :pointer)
     (i :int))
  :returning ((* :char)))

(cffi:defcfun "sentence_nth_word_has_disjunction"
    ((sent :pointer)
     (i :int))
  :returning ((* :char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linkage
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "linkage_create"
    ((index :int)
     (sent :pointer)
     (opts :pointer))
  :returning :pointer)

(cffi:defcfun "linkage_set_current_sublinkage"
    ((linkage :pointer)
     (index :int))
  :returning :int)

(cffi:defcfun "linkage_delete ((linkage :pointer))"
  :returning :void)

(defmacro with-linkage ((name index sent opts) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name (linkage_create ,index ,sent ,opts)))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (linkage_delete ,name)))))

(cffi:defcfun "linkage_get_sentence ((linkage :pointer))"
  :returning :pointer)

(cffi:defcfun "linkage_get_num_sublinkages"
    ((linkage :pointer))
  :returning :int)

(cffi:defcfun "linkage_get_num_words"
    ((linkage :pointer))
  :returning :int)

(cffi:defcfun "linkage_get_num_links"
    ((linkage :pointer))
  :returning :int)

(cffi:defcfun "linkage_get_link_lword"
    ((linkage :pointer)
     (index :int))
  :returning :int)

(cffi:defcfun "linkage_get_link_rword"
    ((linkage :pointer)
     (index :int))
  :returning :int)

(cffi:defcfun "linkage_get_link_length"
    ((linkage :pointer)
     (index :int))
  :returning :int)

(cffi:defcfun "linkage_get_link_label"
    ((linkage :pointer)
     (index :int))
  :returning ((* :char)))

(cffi:defcfun "linkage_get_link_llabel"
    ((linkage :pointer)
     (index :int))
  :returning ((* :char)))

(cffi:defcfun "linkage_get_link_rlabel"
    ((linkage :pointer)
     (index :int))
  :returning ((* :char)))

(cffi:defcfun "linkage_get_link_num_domains"
    ((linkage :pointer)
     (index :int))
  :returning :int)


;;; linkage_get_link_domain_names returns char**, which I don't really
;;; understand how to handle in Lisp. It should be possible to define
;;; a foreign structure something like this:
;;; (cffi:def-foreign-type StringVec (:array (* :char) *))
;;; but I need to know the length.
(cffi:defcfun "linkage_get_link_domain_names"
    ((linkage :pointer))
  :returning :pointer ;; StringVec
  )

;;; linkage_get_words returns char**, which I dont' know how to handle.
(cffi:defcfun "linkage_get_words ((linkage :pointer))"
  :returning ((* (* :char)) (simple-array string (*)))) 

(cffi:defcfun "linkage_get_word ((linkage :pointer)"
				       (index :int))
  :returning ((* :char)))

(cffi:defcfun "linkage_print_links_and_domains"
    ((linkage :pointer))
  :returning :pointer)

(cffi:defcfun "linkage_free_links_and_domains"
    ((str :pointer))
  :returning :void)

(defun links-and-domains-str (linkage)
  "Retrieves and converts to a Lisp string the
  linkage_print_links_and_domains string from LP, and then frees
  it. The converted string is returned, as ordinary Lisp string."
  (let ((foreign-str nil))
    (unwind-protect
	(unless
	    (zerop
	     (setq foreign-str
	       (linkage_print_links_and_domains linkage)))
	  (native-to-string foreign-str))
      (when foreign-str
	(linkage_free_links_and_domains foreign-str)
	nil))))

(cffi:defcfun "linkage_print_constituent_tree"
    ((linkage :pointer)
     (mode :int))
  :returning :pointer)

(cffi:defcfun "linkage_free_constituent_tree_str"
    ((str :pointer))
  :returning :void)

(defun constituent-tree-str (linkage mode)
  "Retrieves and converts to a Lisp string the
  linkage_print_constituent_tree string from LP, and then frees
  it. The converted string is returned, as ordinary Lisp string."
  (let ((foreign-str nil))
    (unwind-protect
	(unless
	    (zerop
	     (setq foreign-str
	       (linkage_print_constituent_tree linkage mode)))
	  (native-to-string foreign-str))
      (when foreign-str
	(linkage_free_constituent_tree_str foreign-str)
	nil))))

(cffi:defcfun "linkage_print_diagram ((linkage :pointer))"
  :returning :pointer)

(cffi:defcfun "linkage_free_diagram"
    ((str :pointer))
  :returning :void)

(defun diagram-str (linkage)
  "Retrieves and converts to a Lisp string the
  linkage_print_diagram string from LP, and then frees
  it. The converted string is returned, as ordinary Lisp string."
  (let ((foreign-str nil))
    (unwind-protect
	(unless
	    (zerop
	     (setq foreign-str
	       (linkage_print_diagram linkage)))
	  (native-to-string foreign-str))
      (when foreign-str
	(linkage_free_diagram foreign-str)
	nil))))

(cffi:defcfun "linkage_print_postscript ((linkage :pointer)"
					       (mode :int))
  :returning :pointer)

(cffi:defcfun "linkage_free_postscript"
    ((str :pointer))
  :returning :void)

(defun postscript-str (linkage mode)
  "Retrieves and converts to a Lisp string the
  linkage_print_diagram string from LP, and then frees
  it. The converted string is returned, as ordinary Lisp string."
  (let ((foreign-str nil))
    (unwind-protect
	(unless
	    (zerop
	     (setq foreign-str
	       (linkage_print_postscript linkage mode)))
	  (native-to-string foreign-str))
      (when foreign-str
	(linkage_free_postscript foreign-str)
	nil))))

(cffi:defcfun "linkage_compute_union ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_unused_word_cost ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_disjunct_cost ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_and_cost ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_link_cost ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_is_canonical ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_is_improper ((linkage :pointer))"
  :returning :int)

(cffi:defcfun "linkage_has_inconsistent_domains"
    ((linkage :pointer))
  :returning :int)

(cffi:defcfun "linkage_get_violation_name"
    ((linkage :pointer))
  :returning ((* :char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PostProcessor
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "post_process_open ((name (* :char) string))"
    :strings-convert nil
    :returning :pointer)

(cffi:defcfun "post_process_close ((pp :pointer))"
  :returning :void)

(cffi:defcfun "linkage_post_process "
    ((linkage :pointer)
     (pp :pointer))
  :returning :void)

(defmacro with-post-processor (name &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name (post_process_open name)))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (post_process_close ,name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constituent Structure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:def-foreign-type CNode
    (:struct (label (* :char))
	     (child (* CNode))
	     (next (* CNode))
	     (start :int)
	     (end :int)))

(cffi:defcfun "linkage_constituent_tree"
    ((linkage :pointer))
  :returning ((* CNode)))

(cffi:defcfun "linkage_free_constituent_tree "
    ((node CNode))
  :returning :void)

;; This could also be just
;; (cffi:slot-value-typed 'CNode :foreign linkage 'label)
(cffi:defcfun "linkage_constituent_node_get_label"
    ((linkage :pointer))
  :returning ((* :char)))

;; This could also be just
;; (cffi:slot-value-typed 'CNode :foreign linkage 'child)
(cffi:defcfun "linkage_constituent_node_get_child"
    ((linkage :pointer))
  :returning ((* CNode)))

;; This could also be just
;; (cffi:slot-value-typed 'CNode :foreign linkage 'next)
(cffi:defcfun "linkage_constituent_node_get_next"
    ((linkage :pointer))
  :returning ((* CNode)))

;; This could also be just
;; (cffi:slot-value-typed 'CNode :foreign linkage 'start)
(cffi:defcfun "linkage_constituent_node_get_start" :int
  (linkage :pointer))

;; This could also be just
;; (cffi:slot-value-typed 'CNode :foreign linkage 'end)
(cffi:defcfun "linkage_constituent_node_get_end" :int
  (linkage :pointer))

(defmacro with-constituent-tree ((name linkage) &body b)
  `(let ((,name nil))
     (unwind-protect
	 (unless
	     (zerop (setq ,name (linkage-constituent-tree ,linkage)))
	   ,@b)
       (when (and ,name (not (zerop ,name)))
	 (linkage-free-constituent-tree ,name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun linkgrammar_get_version :pointer)

(defun linkgrammar-get-version ()
  (foreign-string-to-lisp (linkgrammar_get_version)))

(cffi:defcfun "setlocale" :void 
  (localename :string))
