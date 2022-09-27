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

(defun execute (transaction-function &rest arguments)
  (let ((ret (apply transaction-function arguments)))
    (clobber:log-transaction (cons transaction-function arguments)
			               *transaction-log*)
    ret))

(defun init-clobber-journal (journal)
  (setf *journal* journal))

(defun start-clobber (filename name)
  (setf *journal* nil)
  (setf *transaction-log*
	(clobber:open-transaction-log
	 filename
	 (lambda (transaction)
	   (apply (car transaction) (cdr transaction)))))
    (unless *journal*
      (let ((journal (make-instance 'journal :name name)))
        (execute 'init-clobber-journal journal))))

(defun stop-clobber ()
  (clobber:close-transaction-log *transaction-log*))

