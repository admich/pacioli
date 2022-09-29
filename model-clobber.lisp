;;;; Persistence with Clobber
(in-package :pacioli-model)

(defparameter *transaction-log* nil)
(defparameter *journal* nil)

(clobber:define-save-info general-account
  (:name name)
  (:parent parent))

(clobber:define-save-info lt:timestamp
  (:day lt:day-of)
  (:sec lt:sec-of)
  (:nsec lt:nsec-of))

(clobber:define-save-info transaction
  (:date date)
  (:name name)
  (:tags tags)
  (:note note))

(clobber:define-save-info entry
  (:account account)
  (:amount amount)
  (:tags tags)
  (:note note)
  (:reconciled reconciled))

(clobber:define-save-info single-commodity-amount
  (:value value)
  (:commodity commodity))

(clobber:define-save-info price
  (:date date)
  (:value amount))

(defparameter *log-comment* "")
(defmacro with-log-comment (comment &body body)
  `(let ((*log-comment* ,comment))
     ,@body))

(defclass log-transaction ()
  ((%function-name :initarg :function-name :reader function-name)
   (%arguments :initarg :arguments :reader arguments)
   (%creation-date :initform (lt:now)
		   :initarg :creation-date
		   :reader creation-date)
   (%comment :initform *log-comment* :initarg :comment :reader comment)))

(clobber:define-save-info log-transaction
  (:function-name function-name)
  (:arguments arguments)
  (:creation-date creation-date)
  (:comment comment))

(defparameter *log-transactions* '())

(defun execute (transaction-function &rest arguments)
  (let ((ret (apply transaction-function arguments)))
    (when *transaction-log*
      (let ((log-transaction (make-instance 'log-transaction
		       :function-name transaction-function
		       :arguments arguments)))
        (push log-transaction *log-transactions*)
        (clobber:log-transaction log-transaction
			                     *transaction-log*)))
    ret))

(defun init-clobber-journal (journal)
  (setf *journal* journal))

(defun start-clobber (filename name)
  (setf *journal* '())
  (setf *log-transactions* '())
  (setf *transaction-log*
	(clobber:open-transaction-log
	 filename
	 (lambda (transaction)
       (push transaction *log-transactions*)
	   (apply (function-name transaction)
		  (arguments transaction)))))
    (unless *journal*
      (let ((journal (make-instance 'journal :name name)))
        (execute 'init-clobber-journal journal))))

(defun stop-clobber ()
  (when *transaction-log*
    (clobber:close-transaction-log *transaction-log*))
  (setf *transaction-log* nil
        *journal* nil))

