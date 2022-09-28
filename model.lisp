(in-package #:pacioli-model)
;;;; commodity
(defparameter *main-commodity* :EUR)
(defparameter *commodities* (list *main-commodity*))
(defparameter *prices* '())
(defparameter *possible-tags* '())

(defun modify-object (obj accessor new-value)
  (funcall (fdefinition (list 'setf accessor))
                     new-value obj))

(defclass price ()
  ((%date :initarg :date :accessor date :initform (lt:today))
   (%amount :initarg :value :accessor amount)))

(defmethod print-object ((object price) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a ~a)" (date object) (amount object))))

(defun add-commodity (commodity)
  (unless (find commodity *commodities*)
    (pushnew commodity *commodities*)
    (setf (getf *prices* commodity) '())))

(defun new-price (commodity price)
  (push price (getf *prices* commodity))
  (setf (getf *prices* commodity)
        (sort (getf *prices* commodity) #'lt:timestamp> :key #'date)))

(defun commodity-prices (commodity)
  (getf *prices* commodity))

(defmethod number-of-prices (commodity)
  (length (commodity-prices commodity)))

(defmethod last-price (commodity)
  (first (commodity-prices commodity)))

;;;; amount
(defclass amount ()
  ())

(defclass single-commodity-amount (amount)
  ((%value :initarg :value :accessor value)
   (%commodity :initarg :commodity :accessor commodity)))

(defmethod print-object ((object single-commodity-amount) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a ~a)" (value object) (commodity object))))

(defclass multi-commodity-amount (amount)
  ((%amounts :initarg :amounts :accessor amount-amounts :initform '())))

(defmethod amount-amounts ((amount single-commodity-amount))
  (list amount))

(defmethod add-amounts (amount1 amount2)
  (let ((final-amounts '())
        (amounts (concatenate 'list
                              (amount-amounts amount1)
                              (amount-amounts amount2))))
    (loop for amount in amounts do
      (a:if-let (tot-amount (find (commodity amount)
                                  final-amounts :key #'commodity))
        (setf (value tot-amount)
              (+ (value tot-amount) (value amount)))
        (push (make-instance 'single-commodity-amount
                             :commodity (commodity amount)
                             :value (value amount))
              final-amounts)))
    (cond
      ((> (length final-amounts) 1)
       (make-instance 'multi-commodity-amount :amounts final-amounts))
      ((null final-amounts) (make-instance 'multi-commodity-amount))
      (t (first final-amounts)))))

(defmethod convert-amount (amount)
  (loop with total = (make-instance 'single-commodity-amount :value 0 :commodity *main-commodity*)
        for x in (amount-amounts amount) do
       (setf total (add-amounts total (convert-amount x)))
     finally (return total)))

(defmethod convert-amount ((amount single-commodity-amount))
  (cond  ((eql *main-commodity* (commodity amount))
          amount)
         ((null (commodity amount))
          (make-instance 'single-commodity-amount
                       :value 0
                       :commodity *main-commodity*))
         (t
          (let ((factor (value (amount
                                   (last-price (commodity amount))))))
            (make-instance 'single-commodity-amount
                           :value (* (value amount) factor)
                           :commodity *main-commodity*)))))

(defmethod amount-zerop ((amount multi-commodity-amount))
  (every #'amount-zerop (amount-amounts amount)))

(defmethod amount-zerop ((amount single-commodity-amount))
  (zerop (value amount)))


;;;; account
(defclass name-mixin ()
  ((%name :initarg :name :accessor name)))

(defclass general-account (name-mixin)
  ((%parent :initarg :parent :accessor parent :initform nil)
   (%children :initarg :children-accounts :accessor children :initform '())))

(defclass account (general-account)
  ())

(defmethod print-object ((object account) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a)" (name object))))

(defun register-account (account)
  (push account (children (parent account))))

(defun rename-account (account name)
  (setf (name account) name))

(defclass journal (account)
  ((%transactions :initarg :transactions :accessor transactions :initform '())))

(defmethod long-name ((account journal))
  (name account))

(defmethod long-name ((account general-account))
  (if (typep (parent account) 'journal)
      (name account)
      (concatenate 'string (long-name (parent account)) ":" (name account))))

(defmethod  journal ((account journal))
  account)

(defmethod  journal ((account account))
  (when (parent account)
    (journal (parent account))))

(defmethod journal-commodities ((journal journal))
  *commodities*)

(defmethod children-accounts ((obj (eql nil)))
  nil)

(defmethod subaccounts (account)
  (loop :for acc :in (children account)
        :append `(,acc ,@(subaccounts acc))))


;;;; entry
(defclass tags-mixin ()
  ((%tags :initarg :tags :accessor tags :initform nil)))

(defclass note-mixin ()
  ((%note :initarg :note :accessor note :initform nil :type (or null string))))

(defclass entry (tags-mixin note-mixin)
  ((%account :initarg :account :accessor account :type account)
   (%amount :initarg :amount :accessor amount :type amount)
   (%transaction :accessor transaction :type transaction)
   (%reconciled :initarg :reconciled
                :accessor reconciled
                :initform nil
                :type (member t nil :pending))))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a ~a)"  (account object) (amount object))))

(defmethod date ((object entry))
  (date (transaction object)))

(defmethod name ((object entry))
  (name (transaction object)))

(defclass transaction (name-mixin tags-mixin note-mixin)
  ((%date :initarg :date :initform (local-time:today) :accessor date)
   (%entries :initform '() :initarg :entries :accessor entries)))

(defmethod print-object ((object transaction) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~a ~a)" (date object) (name object))))

(defmethod register-transaction (journal transaction)
  (push transaction (transactions journal))
  (setf (transactions journal) (sort (transactions journal) #'lt:timestamp> :key #'date)))

(defmethod register-entries (transaction entries)
  (mapc (lambda (x) (setf (transaction x) transaction)) entries)
  (setf (entries transaction) (append entries (entries transaction))))

(defmethod register-entries* (transaction &rest entries)
  (register-entries transaction entries))

(defmethod delete-transaction (transaction journal)
  (setf (transactions journal) (delete transaction (transactions journal))))

(defmethod delete-entry (entry)
  (let ((transaction (transaction entry)))
    (setf (entries transaction) (delete entry (entries transaction)))))

(defun find-account (journal fullname)
  (loop
     with names = (ppcre:split ":" fullname)
     with account
     with accounts = (children journal)
     while names do
       (setf account (find (pop names) accounts :test #'string= :key #'name))
       (setf accounts (children account))
     finally (return account)))

(defun account-ancestor-p (parent child)
  (when (and child parent (not (eql child :root)))  
    (or (eql parent child)
        (account-ancestor-p parent (parent child)))))

(defun transaction-of-account-p (transaction account)
  "Return T if transaction contain ACOUNT or one of his subaccounts"
  (find account (map 'list #'account (entries transaction)) :test #'account-ancestor-p))

(defmethod transactions ((account account))
    "Return the transaction in which is involved the account ACCOUNT"
  (sort (loop for x in (transactions (journal account))
      when (transaction-of-account-p x account)
      collect x)  #'local-time:timestamp< :key #'date))

(defgeneric amount-for-account (transaction account &key reconciled))

(defmethod amount-for-account (transaction account &key reconciled)
  (loop
    :with total = (make-instance 'multi-commodity-amount)
    :for x :in (entries transaction)
    :when (and (account-ancestor-p account (account x))
               (or (null reconciled) (reconciled x))) :do
       (setf total (add-amounts total (amount x)))
     :finally (return total)))

(defgeneric balance (account &key time reconciled))

(defmethod balance ((account  general-account) &key time reconciled)
  (declare (ignore time))
  (loop :with total = (make-instance 'multi-commodity-amount)
        :for x :in (transactions account) :do
          (setf total (add-amounts
                       total
                       (amount-for-account x account :reconciled reconciled)))
     :finally (return total)))


;;;; virtual accounts
(defclass virtual-account (general-account)
  ((fn-transaction-p :initarg :fn-transaction-p
                     :accessor fn-transaction-p
                     :initform (lambda (transaction) nil)
                     :documentation "A function of one argument that return T if the argument is a transaction of the virtual-account")
   (fn-amount-for-account :initarg :fn-amount-for-account
                          :accessor fn-amount-for-account
                          :initform (lambda (transaction) (make-instance 'multi-commodity-amount))
                          :documentation "A function of one argument that return the amount for the account in the transaction in the argument")))

(defmethod transactions-sink ((account virtual-account))
  "Return the transactions from which select the transactions 
of the account ACCOUNT"
  (a:when-let ((parent (parent account)))
    (transactions parent)))

(defmethod transactions ((account virtual-account))
  "Return the transaction in which is involved the account ACCOUNT"
  (let ((fn (fn-transaction-p account)))
    (sort (loop for x in (transactions-sink account)
             when (funcall fn x)
             collect x)  #'local-time:timestamp< :key #'date)))

(defmethod amount-for-account (transaction (account virtual-account) &key &allow-other-keys)
  (let ((fn (fn-amount-for-account account)))
    (funcall fn transaction)))

(defun make-join-account (&rest accounts)
  (make-instance 'virtual-account
                 :parent (journal (car accounts))
                 :name (format nil "~{~a~^+~}" (map 'list #'long-name accounts))
                 :fn-transaction-p (lambda (transaction)
                                     (loop for account in accounts
                                        thereis (transaction-of-account-p transaction account)))
                 :fn-amount-for-account (lambda (transaction)
                                          (loop :with total = (make-instance 'multi-commodity-amount)
                                                :for account :in accounts do
                                                  (setf total (add-amounts total (amount-for-account transaction account)))
                                                finally (return total)))))

(defun transaction-broken-p (transaction)
  (not (amount-zerop (total-amount transaction))))

(defun total-amount (transaction)
  (loop :with total = (make-instance 'multi-commodity-amount)
        :for x :in (entries transaction) :do
          (setf total
                (add-amounts
                 total
                 (amount x)))
        :finally (return total)))

