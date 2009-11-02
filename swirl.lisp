(in-package :smart)

;; 
;; PARSER API
;;

(defun swirl-parse-string (string)
  "Primary parsing interface for simple strings"
  (tell-swirl (convert-to-swirl-format string))
  (read-parse-tree (swirl-response)))

(defun debug-swirl-parse-string (string)
  "Parse, but return unparsed string from Swirl"
  (tell-swirl (convert-to-swirl-format string))
  (swirl-response))

;; 
;; API Support
;;

(defun convert-to-swirl-format (string)
  (simple-tokenizer string))

(defun read-parse-tree (swirl-string)
  (let ((escaped (escape-string-for-reader swirl-string)))
    (multiple-value-bind (parse-tree pos) 
	(read-from-string escaped)
      (values parse-tree (subseq escaped pos)))))


(defparameter *punctuation* '(#\? #\! #\. #\, #\; #\: #\) #\( ))

(defun simple-tokenizer (string)
  "Adds whitespace to all tokens, except in special circumstances"
  (with-string-stream (result)
    (labels ((prior-char (index)
	       (unless (< (1- index) 0)
		 (char string (1- index))))
	     (next-char (index)
	       (unless (>= (1+ index) (length string))
		 (char string (1+ index))))
	     (add-prior-space (char i)
	       (when (member (char string i) *punctuation*)
		 (unless (and (eq char #\.)
			      (and (not (null (prior-char i)))
				   (digit-char-p (prior-char i)))
			      (and (not (null (next-char i)))
				   (digit-char-p (next-char i))))
		   (write-char #\Space result))))
	     (add-post-space (char i)
	       (when (member (char string i) *punctuation*)
		 (unless (and (eq char #\.)
			      (and (not (null (prior-char i)))
				   (digit-char-p (prior-char i)))
			      (and (not (null (next-char i)))
				   (digit-char-p (next-char i))))
		   (write-char #\Space result)))))
      (loop for i from 0 below (length string) do
	   (let ((char (char string i)))
	     (add-prior-space char i)
	     (write-char char result)
	     (add-post-space char i))))))

(defun escape-string-for-reader (string)
  "Takes simple "
  (cl-ppcre:REGEX-REPLACE-ALL "([\\.\\,\\;\\:])" string "\\\\\\&"))

;; ========================
;; CONFIGURATION
;; ========================

(defparameter *swirl-bin* "/usr/local/bin/swirl_parse_classify")
(defparameter *swirl-parse-model* "/Users/eslick/Work/fsrc/unix/swirl-1.1.0/model_charniak/")
(defparameter *swirl-role-model* "/Users/eslick/Work/fsrc/unix/swirl-1.1.0/model_swirl/")
(defparameter *swirl-prompt* "SRL>")

;; ========================
;; SWIRL PROCESS IO
;; ========================

(defvar *swirl-instream* nil)
(defvar *swirl-outstream* nil)
(defvar *swirl-errstream* nil)
(defvar *swirl-proc* nil)

(defun tell-swirl (string)
  (ensure-swirl-stream)
  (format *swirl-instream* "3 ~A~%" string)
  (force-output *swirl-instream*))

(defun swirl-response ()
  (wait-for-prompt 5))

;; details

(defun ensure-swirl-stream ()
  (unless (and (valid-stream-p *swirl-instream*) 
	       *swirl-proc*)
    (reset-swirl-stream)))
  
(defun valid-stream-p (value)
  (and (streamp value) (open-stream-p value)))

(defun open-swirl-stream ()
  (multiple-value-bind (proc)
      (ccl:run-program *swirl-bin* 
		       (list (format nil "~A" *swirl-role-model*)
			     (format nil "~A" *swirl-parse-model*))
		       :input :stream
		       :output :stream
		       :error nil
		       :wait nil)
    (setf *swirl-proc* proc
	  *swirl-errstream* (ccl:external-process-error-stream proc)
	  *swirl-instream* (ccl:external-process-input-stream proc)
	  *swirl-outstream* (ccl:external-process-output-stream proc))
    (wait-for-prompt 30))
  :open)

(defun wait-for-prompt (&optional (timeout-seconds 0))
  (with-string-stream (out)
    (unless (accumulate-until-match-or-timeout *swirl-outstream* out 
					       *swirl-prompt* timeout-seconds)
      (error "Search for swirl prompt ~A with timeout ~A timed out" 
	     *swirl-prompt* *swirl-outstream* timeout-seconds))))
					       
(defun accumulate-until-match-or-timeout (in out prompt timeout)
  (let ((start-time (get-universal-time))
	(match-end (length prompt)))
    (labels ((rec (state)
	       (let ((char (read-char-no-hang in nil nil)))
		 (cond ((timeout-p start-time timeout)
			(return-from accumulate-until-match-or-timeout nil))
		       ((null char)
			(sleep 0.25)
			(rec state))
		       (t (write-char char out)
			  (rec (new-state state char))))))
	     (new-state (state char)
	       (if (char= char (char prompt state))
		   (if (= (1+ state) match-end)
		       (return-from accumulate-until-match-or-timeout t)
		       (1+ state))
		   0)))
      (rec 0))))

(defun timeout-p (start-time timeout)
  (when (not (= timeout 0))
    (> (get-universal-time) (+ start-time timeout))))

(defun close-swirl-stream ()
  (when *swirl-proc*
    (shell-kill *swirl-proc*)
    (setf *swirl-proc* nil))
  (when (valid-stream-p *swirl-outstream*)
    (close *swirl-outstream*)
    (setf *swirl-outstream* nil))
  (when (valid-stream-p *swirl-instream*)
    (close *swirl-instream*)
    (setf *swirl-instream* nil))
  (when (valid-stream-p *swirl-errstream*)
    (close *swirl-errstream*)
    (setf *swirl-errstream* nil))
  :closed)

(defun reset-swirl-stream ()
  (close-swirl-stream)
  (open-swirl-stream))

(defmethod shell-kill (proc)
  (ignore-errors
    (ccl:signal-external-process proc 9)))
		     
;; =========================
;; TESTING
;; =========================

(defun swirl-test ()
  (print (swirl-parse-string *test-phrase*)))

(defparameter *test-phrase*
  "The happy General abandoned the beseiged fort .")

(defparameter *test-parse* 
  "( S1 0 
    ( S 1 
     ( NP 2 { B-A0-3 }
      ( DT 0 the the 0  )
      ( JJ 1 happy happy 0  )
      ( NNP 2 General general 0  ) )
        ( VP 0 
            ( VBD 3 abandoned abandon 0  )
            ( NP 2 { B-A1-3 }
                ( DT 4 the the 0  )
                ( JJ 5 beseiged beseiged 0  )
                ( NNP 6 fort fort 0  ) ) )
        ( \\. 7 \\. \\. 0  ) ) )")




