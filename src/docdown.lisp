(in-package :docdown)

;;;  finding/registering docnode classes with the system
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *docnode-class-aliases* nil
    "Alist that maps a symbol to a docnode class.")
  
  (defun register-docnode-class (class &key aliases)
    (map nil #'(lambda (alias)
		 (setf *docnode-class-aliases*
		       (cons (cons alias class)
			     (remove alias *docnode-class-aliases* :key #'car))))
	 aliases)))

;;; Finding docnodes by name
(defgeneric docnode-name (docnode))

(defparameter *docnodes* nil
  "List of active docnodes.")

(defun find-docnode (name)
  (find name *docnodes* :key #'docnode-name) :test #'equal)

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
   (content :initarg :content :initform nil :accessor docnode-content)))

;;; symbolic
(define-layered-class symbolic-docnode
  (standard-docnode)
  ((subject-symbol :initarg :subject-symbol :initform nil :accessor docnode-subject-symbol)))

(define-layered-class function-docnode
  (symbolic-docnode)
  ())

;;; markdown docnode
(define-layered-class markdown-docnode
  (abstract-docnode)
  ((markdown :initarg :markdown :initform nil :accessor docnode-markdown)))

;;;; Introducing the HTML-GENERATION-LAYER
(deflayer html-generation-layer)

(defun generate-html-page (root-node)
  (with-active-layers (html-generation-layer)
    (doc root-node)))

(define-layered-class page-docnode
  :in-layer html-generation-layer
  (standard-docnode)
  ())
(register-docnode-class (find-class 'page-docnode) :aliases '(:page))

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
  :in-layer html-generation-layer ((page page-docnode) &key &allow-other-keys)
  )

(define-layered-method doc
  :in-layer html-generation-layer ((node markdown-docnode) &key &allow-other-keys)
  (with-output-to-string (stream)
    (markdown:markdown (docnode-markdown node) :stream stream)))

(define-layered-method doc
  :in-layer html-generation-layer ((node string) &key &allow-other-keys)
  node)


(define-layered-method doc
  :in-layer html-generation-layer ((symbol symbol) &key &allow-other-keys)
  (if symbol
      (or (find-docnode symbol)
	  (error "Did not find docnode named ~S" symbol))
      ""))

(defmacro docdown (&body forms)
  (labels ((transform-form (form)
	     (cond
	       ((stringp form) `(make-instance 'markdown-docnode :markdown ,form))
	       ((atom form) form)
	       ((keywordp (car form)) (error "Not yet supported."))
	       (t form))))
    `(list ,@(mapcar #'transform-form forms))))

      

;(defmacro defdoc (node-name doc-type-symbol &body options)
  


;(defdoc index :page
;  ()
;  "
;"