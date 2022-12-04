(in-package #:pacioli)

(defparameter *scheduled-transactions* nil)

(defclass scheduled-transaction (transaction)
  ((%scheduling :initarg :scheduling :initform :monthly :accessor scheduling)
   (%last-creation :initarg :last-creation :initform nil :accessor last-creation)))

(clobber:define-save-info scheduled-transaction
  (:scheduling scheduling)
  (:last-creation last-creation))

(defmethod update-last-creation (transaction date)
  (setf (last-creation transaction) date))


;;; presentations
(define-presentation-method present (object (type scheduled-transaction) stream view &key)
  (let ((*standard-output* stream))
    (write-string (name object))))

;;;; scheduled transaction
(defclass view-scheduled-transactions (textual-view) ())

(define-pacioli-command (com-view-scheduled-transactions :name t) ()
  (set-main-view (make-instance 'view-scheduled-transactions)))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view view-scheduled-transactions))
  (let ((*standard-output* pane))
    (format t "Scheduled Transaction:~%")
    (dolist (x *scheduled-transactions*)
      (present x 'scheduled-transaction))))

(defun new-scheduled-transaction (transaction)
  (push transaction *scheduled-transactions*))

(define-pacioli-command (com-new-scheduled-transaction :name t)
    ((model-transaction 'transaction))
  (with-accessors ((name pacioli-model::name) (date pacioli-model::date)
                   (entries pacioli-model::entries) (tags pacioli-model::tags)
                   (note pacioli-model::note)) model-transaction
    (pm:execute 'new-scheduled-transaction
                (make-instance 'scheduled-transaction
                               :date date
                               :last-creation date
                               :name name
                               :tags (copy-list tags) :note note
                               :entries (map 'list #'clone-entry entries)))))

(defmethod scheduled-transaction-dates% (transaction (scheduling (eql :monthly)) start-date &optional (stop-date (lt:today)))
  (let* ((date (pacioli-model:date transaction))
         (day (lt:timestamp-day date))
         (start-date (or start-date date)))
    (loop :for months := 1 :then (+ 1 months)
          :for maybe-date := (lt:timestamp+ date months :month)
          :while (lt:timestamp<= maybe-date stop-date)
          :when (lt:timestamp< start-date maybe-date)
            :collect maybe-date)))

(defun scheduled-transaction-dates (transaction)
  (scheduled-transaction-dates%
   transaction
   (scheduling transaction)
   (or (last-creation transaction) (lt:timestamp- (date transaction) 1 :day))
   (lt:today)))

(defun create-scheduled-transactions (transaction)
  (loop :for date :in  (scheduled-transaction-dates transaction)
        :collect
        (let ((entries (map 'list #'clone-entry (entries transaction))))
          (make-instance 'transaction
                         :entries entries
                         :date date
                         :name (name transaction)
                         :tags (cons :SCHEDULED (copy-list (tags transaction)))
                         :note (note transaction)))))
                       
(define-pacioli-command (com-create-scheduled-transactions :name t)
    ()
  (dolist (tr *scheduled-transactions*)
    (loop :for sched-tr :in (create-scheduled-transactions tr) :do
      (pm:execute 'pm:register-transaction
                  (current-journal *application-frame*)
                  sched-tr)
          :finally (pm:execute 'update-last-creation tr (date sched-tr)))))
       

