(in-package :pacioli)
;;;; timestamp
(define-presentation-type-abbreviation timestamp ()
  '((adm-clim-lib:timestamp) :format (:year "-" (:month 2) "-" (:day 2))))
;;;; VIEWS

(defclass view-commodities (view) ())

(defclass view-account (textual-view)
  ((%account :initarg :account :reader view-account)))

(defclass view-plot (textual-view)
  ((%graph :initarg :graph :reader view-plot-graph)))

;;;; ACCOUNT

(define-presentation-method present (object (type general-account) stream view &key)
  (declare (ignore view))
  (format stream "~a" (long-name object)))

(define-presentation-method accept ((type general-account) stream view &key)
  (declare (ignore view))
  (let ((obj (completing-from-suggestions (stream :partial-completers '(#\:))
               (dolist (acc (subaccounts (current-journal *application-frame*)))
                 (suggest (long-name acc) acc))
               (dolist (acc (virtual-accounts *application-frame*))
                 (suggest (long-name acc) acc)))))
    obj))

(define-presentation-method accept ((type account) stream view &key)
  (declare (ignore view))
  (let ((obj (completing-from-suggestions (stream :partial-completers '(#\:))
               (dolist (acc (subaccounts (current-journal *application-frame*)))
                 (suggest (long-name acc) acc)))))
    obj))

;;;; TRANSACTION

(define-presentation-method present (object (type transaction) stream view &key)
  (declare (ignore view))
  (format stream "~a" object))

(define-presentation-method present (object (type transaction) stream (view acl:tabular-view) &key)
  (formatting-row (stream)
    (formatting-cell (stream)
      (present (date object) 'timestamp :stream stream))
    (formatting-cell (stream)
      (format stream "~a" (name object)))
    (formatting-cell (stream)
      (format stream "" (note object)))
    (formatting-cell (stream :align-x :right)
      (format stream "~@[[note]~]" (note object))))
  (dolist (entry (entries object))
    (present entry 'entry :stream stream :view acl:+tabular-view+)))

(define-presentation-method accept ((type transaction) stream view &key)
  (declare (ignore view))
  (let ((obj (completing-from-suggestions (stream)
               (dolist (xact (transactions (current-journal *application-frame*)))
                 (suggest (name xact) xact)))))
    obj))
;;;; ENTRY

(define-presentation-method present (object (type entry) stream view &key)
  (declare (ignore view))
  (format stream "~a" object))

(define-presentation-method present (object (type entry) stream (view acl:tabular-view) &key)
  (formatting-row (stream)
    (formatting-cell (stream :align-x :right)
      (present (reconciled object) 'reconcile-status :stream stream))
    (formatting-cell (stream)
      (present (account object) 'account :stream stream))
    (formatting-cell (stream)
      (present (amount object) 'amount :stream stream))
    (formatting-cell (stream :align-x :right)
      (format stream "~@[[note]~]" (note object)))))

(define-presentation-type-abbreviation reconcile-status ()
  `((member-alist (("R" . t) ("P" . :pending) ("N" :value nil)))
    :description "Reconciled status of the entry:"))

;;;; AMOUNT

(define-presentation-type amount () :options ((value :main value-supplied-p (member :main :market :all))))

(define-presentation-method present (object (type amount) stream view &key)
  (declare (ignore view))
  (labels ((present-single (x)
             (format stream "~a ~a" (format-money-number (value x)) (commodity x))))
    (case value
      (:main  (present-single (if (typep object 'single-commodity-amount)
                                  object
                                  (convert-amount object))))
      (:market (present-single (convert-amount object)))
      (:all (dolist (x (amount-amounts object))
               (present-single x)
               (terpri stream))))))

(defun format-money-number (num)
  (multiple-value-bind (int fract) (truncate num)
    (format nil "~:[~;\-~]~:D~A" (and (< num 0) (zerop int)) int (subseq (format nil "~@$" fract) 2))))

;;;; COMMODITY

(define-presentation-type commodity ())

(define-presentation-method present (object (type commodity) stream view &key)
  (format stream "~a" object))
;;;; VALUE
(define-presentation-type value ())

(define-presentation-method present (object (type value) stream view &key)
  (format stream "~a" (format-money-number object)))

(define-presentation-method accept ((type value) stream view &key)
  (declare (ignore view))
  (let ((num  (accept 'real :stream stream :view view :prompt nil)))
    (rationalize num)))

;;;; PRICE

(define-presentation-type price ())

(define-presentation-method present ((object (eql nil)) (type price) stream view &key)
  (format stream "~a" object))

(define-presentation-method present ((object price) (type price) stream view &key)
  (format stream "~a ~a ~a" (date object) (format-money-number (value (amount object))) (commodity (amount object))))


