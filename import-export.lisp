(in-package #:pacioli-model)

(defun import-ledger-prices (path)
   "Import a ledger prices file"
  (ledger:read-journal path))

(defun import-ledger (path &key (name "journal"))
  "Import a ledger file"
  (let* ((journal (ledger:read-journal path))
         (ret (make-instance 'journal :name name)))
    (loop for x in (ledger:journal-contents journal) do
      (import-transaction x ret))
    ret))

(defun import-reconciled (ledger-status)
  (ecase ledger-status
    (:cleared t)
    ((:uncleared nil) nil)
    (:pending :pending)))

(defun import-note-tags (ledger-entry)
  "Return two values: (values note tags)"
  (let* ((note (ledger:entry-note ledger-entry))
         (n (search ":TAGS: " note))
         tags)
    (when n
      (multiple-value-bind (tmp-tags end-tags-position)
          (read-from-string note t nil :start (+ n (length ":TAGS: ")))
        (setf tags tmp-tags
              note (let ((new-note (subseq note end-tags-position)))
                     (if (> (length new-note) 0)
                         new-note
                         nil)))
        (loop :for tag :in tags :do (pushnew tag *possible-tags*))))
    (values note tags)))

(defun import-transaction (ledger-entry journal)
  (let ((date (ledger:entry-date ledger-entry))
        (description (ledger:entry-payee ledger-entry))
        (entries (ledger:entry-transactions ledger-entry))
        (reconciled (import-reconciled (ledger:entry-status ledger-entry))))
    (multiple-value-bind (note tags) (import-note-tags ledger-entry)
      (let ((transaction (make-instance 'transaction
                                        :date date :name description
                                        :note note
                                        :tags tags)))
      (register-entries transaction
                       (map 'list (lambda (x) (import-entry x journal date reconciled)) entries))
      (register-transaction journal transaction)
      (check-for-price transaction)
      transaction))))

(defun check-for-price (transaction)
  (when (loop for entry in (entries transaction)
                thereis
                (not (eql *main-commodity* (commodity (amount entry)))))
  (let ((account-entry '()))
    (loop for entry in (entries transaction) do
      (setf (getf account-entry (account entry))
            (cons (amount entry)
                  (getf account-entry (account entry) '()))))
    (loop for (account . (amounts .rest)) on account-entry by #'cddr
          for main  = (find *main-commodity* amounts :key #'commodity)
          for other = (first (remove main amounts))
          when (and main other (= (length amounts) 2))
            do
               (new-price (commodity other)
                          (make-instance 'price
                                                :value (make-instance 'single-commodity-amount
                                                                      :commodity (commodity main)
                                                                      :value (abs (/ (value main) (value other))))
                                                  :date (date transaction)))))))

(defun import-entry (ledger-transaction journal date reconciled)
  (with-accessors ((account ledger:xact-account)
                   (amount ledger:xact-amount)
                   (status ledger:xact-status)
                   (note ledger:xact-note)) ledger-transaction
    (make-instance 'entry
                   :account (find-or-create-account journal (ledger:account-fullname account))
                   :amount (import-amount amount date)
                   :note note
                   :reconciled (or (import-reconciled status) reconciled))))

(defun import-price (comm date)
  (when (cambl:annotated-commodity-p comm)
    (let* ((annotation (cambl:commodity-annotation comm))
           (amount (import-amount (cambl:annotation-price annotation) date))
           (date (or (cambl:annotation-date annotation) date)))
      (make-instance 'price :value amount :date date))))

(defmethod import-amount ((amount cambl:amount) date)
  (let ((value (cambl:amount-quantity amount))
        (commodity (a:make-keyword (cambl::commodity-symbol-name
                                    (cambl::commodity-symbol
                                     (cambl:amount-commodity amount))))))
    (add-commodity commodity)
    (a:when-let (price (import-price (cambl:amount-commodity amount) date))
      (new-price commodity price))
    (make-instance 'single-commodity-amount :value value
                                            :commodity commodity)))

(defmethod import-amount ((amount number) date)
  (make-instance 'single-commodity-amount :value amount
                                          :commodity nil))

(defun find-or-create-account (journal fullname)
  (loop
     with names = (ppcre:split ":" fullname)
     with account = journal
     with name
     with accounts = (children journal)
     while names do
       (setf name (pop names))
       (setf account (or (find name accounts :test #'string= :key #'name)
                         (car (push (make-instance 'account :name name :parent account) (children (or account journal))))))
       (setf accounts (children account))
     finally (return account)))

;;;; export ledger
(defparameter *export-indent* "    ")
(defun export-ledger (journal path)
  (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (loop for x in (reverse (transactions journal)) do
      (export-transaction x stream))))

(defun export-transaction (transaction stream)
  (lt:format-timestring stream (date transaction)
                        :format '(:year "/" (:month 2) "/" (:day 2) " "))
  (format stream "~a~%" (name transaction))
  (a:when-let ((tags (tags transaction)))
      (format stream "~&~a;" *export-indent*)
      (export-tags tags stream))
  (a:when-let ((note (note transaction)))
    (format stream "~&~a;~a" *export-indent* note))
  (loop for x in (entries transaction) do
    (export-entry x stream))
  (format stream "~%"))

(defun export-reconciled (reconciled)
  (ecase reconciled
      ((nil) nil)
      ((t) "*")
      ((:pending) "!")))

(defun export-entry (entry stream)
  (let* ((account-name (long-name (account entry))))
    (format stream "~&~a~@[~a ~]~a   " *export-indent* (export-reconciled (reconciled entry)) account-name)
    (export-amount (amount entry) stream)
    (a:when-let ((tags (tags entry)))
      (format stream "  ; ")
      (export-tags tags stream))
    (a:when-let ((note (note entry)))
      (format stream "  ; ~a" note))
    (format stream "~%")))

(defun export-amount (amount stream)
  (format stream "~f " (coerce (value amount) 'double-float))
  (export-commodity (commodity amount) stream))

(defun export-commodity (commodity stream)
  (let* ((str (format nil "~a" commodity)))
    (if (find #\. str)
        (format stream "\"~a\"" str)
        (format stream "~a" str))))

(defun export-tags (tags stream)
  (format stream ":TAGS: ~s" tags))
