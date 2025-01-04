(in-package #:parse-argv)

#|
#|EXPORT|#				:convert-value
 |#
(defgeneric convert-value (type string-data default-val)
  (:method (type string-data default-val)
	(declare (ignore string-data default-val))
	(error "Type ~S is NOT specialized to convert-value." type)))

(defmethod convert-value ((type (eql :string)) string-data default-val)
  (or string-data default-val))

(defmethod convert-value ((type (eql :boolean)) string-data default-val)
  (if (null string-data)
	  default-val
	  (cond
		((string-equal string-data  "true")   t)
		((string-equal string-data "false") nil)
		(t (error "invalid input ~S for boolean value." string-data)))))

(defmethod convert-value ((type (eql :integer)) string-data default-val)
  (if (null string-data)
	  default-val
	  (parse-integer string-data)))

(defmethod convert-value ((type (eql :keyword)) string-data default-val)
  (if (null string-data)
	  default-val
	  (intern (string-upcase string-data) :keyword)))





(defun operate-short-option (prm rules params options)
  (let ((len (length prm)))
	(labels ((recur (idx)
			   (if (<= len idx)
				   (values params options)
				   (let* ((flag (char prm idx))
						  (rule (find-if (lambda (lst)
										   (char= (second lst) flag)) rules)))
					 (unless rule
					   (error "invalid option -~A." flag))
					 (destructuring-bind (keyword flag &optional long-option val-type default-val) rule
					   (declare (ignore long-option))
					   (if (null val-type)
						   ;; When without value parameter
						   (progn
							 ;; Add keyword & T to options and continue
							 (setf options (cons t (cons keyword options)))
							 (recur (1+ idx)))
						   ;; When with value parameter
						   (if (< 1 (- len idx))
							   ;; If a trailing string exists, add it as an additional parameter.
							   (let ((val (convert-value val-type
														 (subseq prm (1+ idx)) default-val)))
								 (values params (cons val (cons keyword options))))
							   ;; If a trailing string does not exist, check the next parameter
							   ;; (exists: add, otherwise: error)
							   (let ((val (car params)))
								 ;; If the next parameter does not exist or is “--”.
								 (when (or (null val) (string= val "--"))
								   (setf val nil)
								   ;; Error if default value is not specified
								   (unless default-val
									 (error "option value missing for -~A." flag)))
								 (values (cdr params)
										 (cons (convert-value val-type val default-val)
											   (cons keyword options)))))))))))
	  (recur 1))))


(defun operate-long-option (prm rules params options)
  (let ((with-val-p nil))
	;; If '--param=VALUE' format, use --param and push VALUE to params.
	(let ((idx (position #\= prm)))
	  (when idx
		(setf with-val-p t)
		(push (subseq prm (1+ idx)) params)
		(setf prm (subseq prm 0 idx))))
	;; Count the number of rules where long-option matches forward
	(let* ((len   (length prm))
		   (rule  nil)
		   (count (count-if (lambda (lst)
							  (let ((option (third lst)))
								(when (and option
										   (<= len (length option))
										   (string= prm option :end2 len))
								  (setf rule lst)))) rules)))
	  ;; Error if no match.
	  (when (zerop count)
		(error "invalid option ~A." prm))
	  ;; If two or more match, it is ambiguous error.
	  (when (< 1 count)
		(error "ambiguous option : ~A." prm))
	  ;; The following is the case with 1 match -> 'rule' is matched rule.
	  (destructuring-bind (keyword flag &optional long-option val-type default-val) rule
		(declare (ignore flag))
		(if (null val-type)
			;; If no additional parameters are needed, just add them and exit.
			(progn
			  ;; However, if a --param=VALUE format is destructured at the top, an error occurs.
			  (when with-val-p
				(error "invalid option '~A=~A'." prm (car params)))
			  (values params (cons t (cons keyword options))))
			;; If additional parameters are required, use the following parameters (error if not exists)
			;; MEMO : --param=VALUE format has been processed at the top
			(let ((val (car params)))
			  ;; when the next parameter does not exist or is “--”.
			  (when (or (null val) (string= val "--"))
				(setf val nil)
				;; Error if default value is not specified.
				(unless default-val
				  (error "option value missing for ~A." long-option)))
			  (values (cdr params)
					  (cons (convert-value val-type val default-val)
							(cons keyword options)))))))))


(defun parse-options (params options rules)
  (if (null params)
	  (values params (nreverse options))
	  (let* ((prm (car params))
			 (len (length prm)))
		(if (string= prm "--")
			(values (cdr params) (nreverse options))
			(if (or (<= len 1)
					(char/= #\- (char prm 0))
					(and (<= 3 len) (string= "---" prm :end2 3)))
				(values params (nreverse options))
				(let ((operator (if (char= (char prm 1) #\-)
									#'operate-long-option
									#'operate-short-option)))
				  (multiple-value-setq (params options)
									   (funcall operator prm rules (cdr params) options))
				  (parse-options params options rules)))))))

(defun parse-params (params rules optional-p acc)
  (if (and (null params) (null rules))
	  (nreverse acc)
	  (if (eq (car rules) '&optional)
		  (if optional-p
			  (error "&optional specifier duplicate.")
			  (parse-params params (cdr rules) t acc))
		  (if (eq (car rules) '&rest)
			  (if (not (null (cdr rules)))
				  (error "Invalid &rest specifier.")
				  (parse-params nil nil optional-p (push params acc)))
			  (progn
				(when (and params (null rules))
				  (error "too many parameters."))
				(when (and (null params) rules (not optional-p))
				  (error "too less parameters."))
				(let ((param (car params))
					  (rule  (car rules)))
				  (if (not optional-p)
					  (parse-params (cdr params) (cdr rules) optional-p
									(push (convert-value rule param nil) acc))
					  (let ((rule (if (listp rule) rule (list rule nil))))
						(destructuring-bind (type default) rule
						  (parse-params (cdr params) (cdr rules) optional-p
										(push (convert-value type param default) acc)))))))))))
					
#|
#|EXPORT|#				:parse
 |#
(defun parse (argv option-rules param-rules)
  (multiple-value-bind (params options) (parse-options argv nil option-rules)
	(let ((params (parse-params params param-rules nil nil)))
	  (append params options))))


#|
#|EXPORT|#				:get-argv
 |#
(defun get-argv ()
  ;; Borrowed from apply-argv.
  #+sbcl      (cdr sb-ext:*posix-argv*)
  #+clozure   (cdr (ccl::command-line-arguments))
  #+gcl       (cdr si:*command-args*)
  #+ecl       (loop for i from 1 below (si:argc) collect (si:argv i))
  #+cmu       (cdr extensions:*command-line-strings*)
  #+allegro   (cdr (sys:command-line-arguments))
  #+lispworks (cdr sys:*line-arguments-list*)
  #+clisp      ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-argv not supported for your implementation"))
