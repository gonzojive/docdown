(in-package :docdown)

;;;  finding/registering docnode classes with the system
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *docnode-class-aliases* nil
    "Alist that maps a symbol to a docnode class.")

  
  (defun register-docnode-class (class &key aliases)
    (map nil #'(lambda (alias)
		 (setf *docnode-class-aliases*
		       (cons (cons alias class)
			     (remove alias *docnode-class-aliases* :key #'car))))
	 aliases)))

;;; Finding docnodes by name
(defgeneric docnode-name (docnode))

(defvar *docnodes* nil
  "List of active docnodes.")

(defun find-docnode (name)
  (find name *docnodes* :key #'docnode-name :test #'equal))

(defmacro docdown (&body forms)
  (labels ((transform-form (form)
	     (cond
	       ((stringp form) `(make-instance 'markdown-docnode :markdown ,form))
	       ((atom form) form)
	       ((keywordp (car form)) (error "Not yet supported."))
	       (t form))))
    `(list ,@(mapcar #'transform-form forms))))

(defvar *option-evaluators* nil)

(defmacro defdoc (node-name doc-type-symbol &body options)
  (let ((docnode-class (cdr (assoc doc-type-symbol *docnode-class-aliases*))))
    (with-unique-names (instance)
      `(let ((,instance (make-instance ,docnode-class
				       :name ',node-name
				       ,@(mapcan #'(lambda (option)
						     (let ((evaluator (cdr (assoc (car option) *option-evaluators*))))
						       (if evaluator
							   (copy-list (apply evaluator (cdr option)))
							   (error "No option evaluator found for option named ~S" (car option)))))
						 options))))
	 (setf *docnodes*
	       (cons ,instance (if ',node-name
				   (remove ',node-name *docnodes* :key #'docnode-name)
				   *docnodes*)))
	 ,instance))))

(defmacro define-option-evaluator (option-keyword lambda-list &body body)
  `(push (cons ,option-keyword
	       (lambda (,@lambda-list) ,@body)) *option-evaluators*))

(define-option-evaluator :content (&rest args)
  (list :content `(docdown ,@args)))

(define-option-evaluator :sections (&rest args)
  (list :sections `(docdown ,@args)))

(define-option-evaluator :children (&rest args)
  (list :sections `(docdown ,@args)))

(define-option-evaluator :title (title-string)
  (list :title title-string))

(define-option-evaluator :subject-symbol (val)
  (list :subject-symbol val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Docnode classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Abstract docnode class.  The root of all other docnode classes
(define-layered-class abstract-docnode
  ()
  ())

(defmethod docnode-name ((node abstract-docnode)) nil)

;;; typical 
(define-layered-class standard-docnode
  (abstract-docnode)
  ((subject :initarg :subject :initform nil :accessor docnode-subject)
   (name :initarg :name :initform nil :accessor docnode-name)
   (title :initarg :title :initform nil :accessor docnode-title)
   (content :initarg :content :initform nil :accessor docnode-content)))

(defmethod print-object ((node standard-docnode) s)
	   (print-unreadable-object (node s :type t :identity nil)
	     (format s "~S" (docnode-name node))))

;;; symbolic
(define-layered-class symbolic-docnode
  (standard-docnode)
  ((subject-symbol :initarg :subject-symbol :initform nil :accessor docnode-subject-symbol)))

(defmethod initialize-instance :after ((node symbolic-docnode) &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (format t "SYMBOLIC: ~A ~A" (docnode-subject-symbol node) (docnode-subject node))
  (when (not (docnode-subject-symbol node))
    (setf (docnode-subject-symbol node)	  (docnode-name node)))
  (when (not (docnode-subject-symbol node))
    (awhen (docnode-subject  node)
      (when (symbolp it)
	(setf (docnode-subject-symbol node) it)))))


(define-layered-class function-docnode
  (symbolic-docnode)
  ())

(register-docnode-class (find-class 'function-docnode) :aliases '(:function))

(defmethod initialize-instance :after ((node function-docnode) &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (format t "FUNCTION modified: ~A ~A" (docnode-subject-symbol node) (docnode-subject node))
  (when (not (docnode-subject node))
    (setf (docnode-subject node) (fdefinition (docnode-subject-symbol node)))))

;;; markdown docnode
(define-layered-class markdown-docnode
  (abstract-docnode)
  ((markdown :initarg :markdown :initform nil :accessor docnode-markdown)))

;;;; Introducing the HTML-GENERATION-LAYER
(deflayer html-generation-layer)

(defun generate-html-page (root-node)
  (let ((*docnodes* *docnodes*))
    (with-active-layers (html-generation-layer)
      (doc root-node))))

(deflayer toc-generation-layer)
(deflayer section-generation-layer (html-generation-layer))
;(deflayer html-toc-generation-layer (toc-generation-layer html-generation-layer))

(define-layered-class multisection-docnode
  (standard-docnode)
  ((sections :initarg :sections :initform nil :accessor docnode-sections))
  )

(define-layered-class page-docnode
  :in-layer html-generation-layer
  (multisection-docnode)
  ()
  )
(register-docnode-class (find-class 'page-docnode) :aliases '(:page))

(define-layered-class section-docnode
  :in-layer html-generation-layer
  (multisection-docnode)
  ()
  )

(register-docnode-class (find-class 'section-docnode) :aliases '(:section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; TOC, the function.   (Table of Contents)
;;;;; and the cdr is a list of (TOC sections-of-the-node)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-layered-class toc-entry
  ()
  ((node :initform nil :initarg :node :accessor toc-entry-node)
   (children :initform nil :initarg :children :accessor toc-entry-children)))

(defmethod print-object ((entry toc-entry) s)
	   (print-unreadable-object (entry s :type nil :identity nil)
	     (format s "~S ~S" (toc-entry-node entry) (toc-entry-children entry))))

(defgeneric toc-entry-title (entry)
  (:method ((entry toc-entry))
	   (or 
	    (docnode-title (toc-entry-node entry))
	    (string-downcase (symbol-name (docnode-name (toc-entry-node entry)))))))
		      
(define-layered-function toc (node &key &allow-other-keys))

(define-layered-method toc ((node standard-docnode) &key &allow-other-keys)
  (when (docnode-title node)
    (make-instance 'toc-entry :node node)))


(define-layered-method toc ((node symbolic-docnode) &key &allow-other-keys)
  (make-instance 'toc-entry :node node))

(define-layered-method toc
  ((node multisection-docnode) &key &allow-other-keys)
  (let ((entry (or (call-next-layered-method)
		   (make-instance 'toc-entry :node node))))
    (setf (toc-entry-children entry)
	  (mapcar #'toc (docnode-sections node)))
    entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; DOC, the function.  Generates some sort of output documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-layered-function doc (thing &key &allow-other-keys))

(define-layered-method doc
  ((list cons) &rest rest &key &allow-other-keys)
  (mapcar #'(lambda (elem) (apply #'doc elem rest)) list))

(define-layered-method doc
  :in-layer html-generation-layer ((list cons) &key &allow-other-keys)
  (apply #'concatenate 'string (call-next-layered-method) ))


(define-layered-method doc
  :in-layer html-generation-layer ((node  standard-docnode) &key &allow-other-keys)
  (with-html-output-to-string (s)
    (:a :name (anchor-id node))
    (str (doc (docnode-content node)))))

(defvar *section-depth* 1)

(define-layered-method doc
  :in-layer html-generation-layer ((page page-docnode) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (:html
     (:head
      (:title (fmt "~A | DocDown document" (docnode-title page)))
      (:style :type "text/css"
	      (esc (documentation-css))))
     (:body
      (:h1 (esc (docnode-title page)))
      (str (call-next-layered-method))))))

(define-layered-function output-toc? (node &key &allow-other-keys))
(define-layered-method output-toc? (node &key &allow-other-keys)
		       nil)
(define-layered-method output-toc? ((node page-docnode) &key &allow-other-keys)
		       t)

(define-layered-method doc
  :in-layer html-generation-layer ((node multisection-docnode) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (str (doc (docnode-content node)))
    (:a :name (anchor-id node))
    
    (when (output-toc? node)
      (htm
       (:h2 "Contents")
       (:ol
	(str (doc (toc-entry-children (toc node)))))))

    (let ((*section-depth* (+ *section-depth* 1)))
      (map nil #'(lambda (section)
		   (fmt "<h~A>" *section-depth*)
		   (esc (docnode-title section))
		   (fmt "</h~A>" *section-depth*)
		   (str (doc section)))
	   (docnode-sections node)))))

(define-layered-function anchor-id (node &key &allow-other-keys))
(define-layered-method anchor-id :in-layer html-generation-layer ((node standard-docnode) &key &allow-other-keys)
		       (awhen (docnode-name node)
			 (string-downcase (symbol-name it))))
		       

(define-layered-method doc
  :in-layer html-generation-layer ((entry toc-entry) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (:li (:a :href (format nil "#~A" (anchor-id (toc-entry-node entry)))
	     (esc (toc-entry-title entry))))
    (when-let (children (toc-entry-children entry))
      (htm
       (:ol
	(map 'list #'(lambda (child) (str (doc child))) children))))))

(define-layered-method doc
  :in-layer section-generation-layer ((node standard-docnode) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (let ((*section-depth* (+ *section-depth* 1)))
      (map nil #'(lambda (section)
		   (fmt "<h~A>" *section-depth*)
		   (esc (docnode-title section))
		   (fmt "</h~A>" *section-depth*)
		   (str (doc section)))
	   (docnode-sections node)))))

(define-layered-method doc
  :in-layer toc-generation-layer ((node multisection-docnode) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (str (doc (docnode-content node)))

    (let ((*section-depth* (+ *section-depth* 1)))
      (map nil #'(lambda (section)
		   (fmt "<h~A>" *section-depth*)
		   (esc (docnode-title section))
		   (fmt "</h~A>" *section-depth*)
		   (str (doc section)))
	   (docnode-sections node)))))


(define-layered-method doc
  :in-layer html-generation-layer ((node markdown-docnode) &key &allow-other-keys)
  (with-output-to-string (stream)
    (markdown:markdown (docnode-markdown node) :stream stream)))

(define-layered-method doc
  :in-layer html-generation-layer ((node string) &key &allow-other-keys)
  node)


(define-layered-method doc
  :in-layer html-generation-layer ((symbol symbol) &key &allow-other-keys)
  (if (null symbol)
      ""
      (aif (find-docnode symbol)
	   (doc it)
	   (error "Did not find docnode named ~S" symbol))))

(define-layered-method doc
  :in-layer html-generation-layer ((node function-docnode) &key &allow-other-keys)
  (with-html-output-to-string (stream)
    (:div "[Function]")
    (:div
     (:strong (esc (string-downcase (symbol-name (docnode-subject-symbol node)))))
     "  "
     (:em
      (fmt "~{~A ~}" (mapcar (compose #'string-downcase #'symbol-name)
			     (function-lambda-list (docnode-subject node))))))
    (:div :class "symdoc"
	  (str (call-next-layered-method)))))

;(defdoc index :page
;  ()
;  "
;"

(defun documentation-css ()
  (with-css-output-to-string (s)
    (:a :text-decoration "none")
    (:a\:hover :text-decoration "underline")
    (:pre :background-color "#ddeeff"
	  :border "1px solid #aaaaff"
	  :padding ".5em"
	  :margin-left "2em")
    (:.symdoc :margin-left "3em")))
     