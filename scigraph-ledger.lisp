;;;; In this file are defined the DATASET and GRAPH classes useful to plot
;;;; ledger account data.
;;;;
;;;; MIXIN FOR BOTH DATASET AND GRAPH
;;;; --------------------------------
;;;;
;;;; - LEDGER-TIME-INTERVAL-MIXIN
;;;; This class have  start and end date.
;;;;
;;;; - LEDGER-QUANTUM-TIME-MIXIN
;;;; The time data are quantized like in the bar graph.
;;;;
;;;; DATASET
;;;; -------
;;;; - LEDGER-ACCOUNT-DATASET-MIXIN: a class with a ledger account in a slot
;;;; - LEDGER-ACCOUNT-DATASET: account and time interval
;;;; - LEDGER-ACCOUNT-BAR-DATASET: ledger-account-dataset with quantum-time
;;;;
;;;; GRAPH
;;;; -----
;;;; - LEDGER-TIME-STEP-MIXIN
;;;; A time step to automatically set the start and end date.
;;;;
;;;; - LEDGER-ACCOUNTS-GRAPH-MIXIN: graph which have account datasets
;;;; - LEDGER-GRAPH: accounts + time-interval
;;;; - LEDGER-BAR-GRAPH
;;;; - LEDGER-PIE-GRAPH
;;;; - LEDGER-TIME-GRAPH

(in-package :graph)

;;;; TIME MIXIN

(defclass ledger-time-interval-mixin ()
  ((start-date :initform '() :initarg :start-date :accessor start-date)
   (end-date :initform '() :initarg :end-date :accessor end-date)))

(defmethod pop-accept-items progn ((self ledger-time-interval-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (setf (start-date self) (accept '(null-or-type adm-clim-lib:timestamp)
                                  :stream menu-stream
                                  :default (start-date self)
                                  :prompt " From date"
                                  :query-identifier 'start-date))
  (terpri menu-stream)
  (setf (end-date self) (accept '(null-or-type adm-clim-lib:timestamp)
                                :stream menu-stream
                                :default (end-date self)
                                :prompt " To date"
                                :query-identifier 'end-date)))

(defclass ledger-quantum-time-mixin ()
  ((time-quanta :initform :year
                :initarg :time-quanta
                :accessor time-quanta
                :type (member :year :month :week :day))))

(defmethod pop-accept-items progn ((self ledger-quantum-time-mixin) MENU-STREAM GRAPH-WINDOW)
  (declare (ignore GRAPH-WINDOW))
  (setf (time-quanta self) (accept '(member :year :month :day :week)
                                   :stream menu-stream
                                   :default (quantum-time self)
                                   :prompt " Time quanta"
                                   :query-identifier 'time-quanta)))

(defparameter *format-timestring-for-quanta*
  '(:year ((:year 4))
    :month ((:year 4) "-" (:month 2))
    :day ((:year 4) "-" (:month 2) "-" (:day 2))
    :week ((:year 4) "-" (:month 2) "-" (:day 2))))

(defun timestamp-quantization-dec (timestamp quantum)
  (case quantum
    (:year (local-time:timestamp-minimize-part timestamp :month))
    (:month (local-time:timestamp-minimize-part timestamp :day))
    (:day (local-time:timestamp-minimize-part timestamp :hour))
    (:week (local-time:timestamp-minimize-part
            (local-time:adjust-timestamp timestamp (offset :day-of-week :monday))
            :hour))
    (t timestamp)))

(defun timestamp-quantization-inc (timestamp quantum)
  (case quantum
    (:year (local-time:timestamp-maximize-part timestamp :month))
    (:month (local-time:timestamp-maximize-part timestamp :day))
    (:day (local-time:timestamp-maximize-part timestamp :hour))
    (:week (local-time:timestamp-maximize-part
            (local-time:adjust-timestamp timestamp (offset :day-of-week :monday))
            :hour))
    (t timestamp)))

(defun timestamp-quantum- (timestamp quantum)
  (if (eq quantum :week)
      (local-time:timestamp- timestamp 7 :day)
      (local-time:timestamp- timestamp 1 quantum)))

(defun timestamp-quantum+ (timestamp quantum)
  (if (eq quantum :week)
      (local-time:timestamp+ timestamp 7 :day)
      (local-time:timestamp+ timestamp 1 quantum)))

;;;; ACCOUNT DATASET

(defclass ledger-account-dataset-mixin ()
  ((account :initform '() :initarg :account :accessor dataset-account)))

(defmethod text-for-datum (graph (dataset ledger-account-dataset-mixin) (datum list))
  (let* ((*print-circle* nil))
    (format nil "~A~%~A EUR"  (local-time:universal-to-timestamp (first datum)) (pacioli::format-money-number (second datum)))))

(defclass ledger-account-dataset (ledger-time-interval-mixin ledger-account-dataset-mixin graph-data adm-clim-lib:zelig)
  ())

(defmethod graph-presentation-type ((self ledger-account-dataset) graph) 'ledger-account-dataset)

(define-presentation-translator dataset-to-account
    (ledger-account-dataset pacioli-model:account :graph :menu t)
    (object)
  (dataset-account object))

(defmethod name ((self ledger-account-dataset))
  (pacioli-model:long-name (dataset-account self)))

(defmethod data ((self ledger-account-dataset))
  "Return a list of datum. Each datum are (timestamp total-in-the-account amount-for-the-transaction)"
  (let ((account (dataset-account self))
        (start-date (start-date self))
        (end-date (end-date self)))
    (loop with total = (make-instance 'pacioli-model:single-commodity-amount :value 0 :commodity pacioli-model:*main-commodity*)
          with datum-timestamp 
          for i in (pacioli-model:transactions (dataset-account self)) do
            (setf total
                  (pacioli-model:add-amounts total
                                             (pacioli-model:convert-amount (pacioli-model:amount-for-account i account)))
                  datum-timestamp
                  (pacioli-model:date i))
          when (and (or (not start-date) (local-time:timestamp<= start-date datum-timestamp))
                    (or (not end-date) (local-time:timestamp<= datum-timestamp end-date)))
            collect (list (local-time:timestamp-to-universal datum-timestamp)
                          (pacioli-model:value total)
                          (pacioli-model:value
                           (pacioli-model:convert-amount (pacioli-model:amount-for-account i account)))))))

(defmethod dataset-summary-function (graph (dataset ledger-account-dataset))
  (if (data dataset)
      (loop for x in (data dataset) summing
                                    (third x))
      0))

;;;; ACCOUNT BAR DATASET

(defclass ledger-account-bar-dataset (ledger-quantum-time-mixin
                                      ledger-account-dataset)
  ()
  (:default-initargs :symbologies '(:bar)))

(defmethod data :around ((self ledger-account-bar-dataset))
  (labels ((x-value (utime)
             (local-time:timestamp-to-universal
              (timestamp-quantization-dec
               (local-time:universal-to-timestamp utime)
               (time-quanta self))))
           (plist-to-alist (list)
             (loop for x on list by #'cddr collect (list (first x) (second x)))))
    (plist-to-alist (loop for datum in (call-next-method)
                          with newdata = '()
                          do
                             (if (getf newdata (x-value (first datum)))
                                 (incf (getf newdata (x-value (first datum))) (third datum))
                                 (setf (getf newdata (x-value (first datum))) (third datum)))
                          finally (return  newdata)))))

(defmethod datum-displayer :around ((self ledger-account-bar-dataset) (graph graph-datasets-mixin))
  (let* ((f (call-next-method self graph))
         (bar-width (or (bar-width self) 10))
         (offset (* bar-width (position self (datasets graph)))))
    (declare (compiled-function f))
    (if (zerop offset)
        f
        #'(lambda (stream r s datum)
            (funcall f stream (+ r offset) s datum)))))

;;;; LEDGER GRAPH

(defclass ledger-accounts-graph-mixin ()
  ((accounts :initform '() :initarg :accounts :accessor graph-accounts)))

(defclass ledger-graph (ledger-accounts-graph-mixin
                        ledger-time-interval-mixin
                        annotated-graph
                        adm-clim-lib:zelig)
  ())

(defmethod initialize-instance :after ((self ledger-graph) &key &allow-other-keys)
  (dolist (dataset (datasets self))
    (setf (start-date dataset) (start-date self)
          (end-date dataset) (end-date self))))

(defmethod graph-presentation-type ((self ledger-graph) graph) 'ledger-graph)

(define-presentation-translator graph-ledger
    (ledger-graph pacioli-model:account :graph :menu t)
    (object)
  (first (graph-accounts object)))

(defmethod (setf start-date) :after (new-value (self ledger-graph))
  (dolist (dataset (datasets self))
    (setf (start-date dataset) (start-date self))))

(defmethod (setf end-date) :after (new-value (self ledger-graph))
  (dolist (dataset (datasets self))
    (setf (end-date dataset) (end-date self))))

(defmethod display :after ((graph ledger-graph)  stream)
  (setf (stream-cursor-position stream) (values (left graph) (+ 100 (top graph) (height graph))))
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream) (format stream "From: "))
      (formatting-cell (stream) (present (start-date graph) '(null-or-type adm-clim-lib:timestamp) :stream stream)))
    (formatting-row (stream)
      (formatting-cell (stream) (format stream "~&To: "))
      (formatting-cell (stream) (present (end-date graph) '(null-or-type adm-clim-lib:timestamp) :stream stream)))))

;;;; LEDGER BAR GRAPH 

(defclass ledger-bar-graph (x-timestamp-graph-mixin ledger-quantum-time-mixin ledger-graph) 
  ()
  (:default-initargs :x-tick-numbering :each
                     :x-format-timestring '((:year 4))
                     :x-auto-tick t
                     :x-dtick (* 365 86400)))

(defmethod initialize-instance :after ((instance ledger-bar-graph) &rest initargs)
  (setf (graph-accounts instance) (graph-accounts instance)
        (time-quanta instance) (time-quanta instance)))

(defmethod (setf graph-accounts) :after (new-value (self ledger-bar-graph))
  (setf (datasets self)
        (map 'list
             (lambda (x) (make-instance 'ledger-account-bar-dataset
                                        :account x))
             new-value)))

(defmethod graph-auto-scale-limits :around ((self ledger-bar-graph))
  (multiple-value-bind (xmin xmax ymin ymax) (call-next-method)
    (when (start-date self)
      (setf xmin (- (local-time:timestamp-to-universal (start-date self)) (/ 15 (x-scale self)))))
    (when (end-date self)
      (setf xmax (local-time:timestamp-to-universal (end-date self))))
    (values xmin xmax (min 0 ymin) (max 0 ymax))))

(defgeneric compute-time-ticks (graph x-min x-max)
  (:documentation "Compute the ticks position for time axis")
  (:method (graph x-min x-max) (* 365 86400)))

(defmethod compute-time-ticks ((graph ledger-bar-graph)  x-min x-max)
  (let* ((time-quanta (time-quanta graph))
         (start-date (timestamp-quantization-dec
                      (local-time:universal-to-timestamp (truncate x-min))
                      time-quanta))
         (end-date (local-time:universal-to-timestamp (truncate x-max)))
         (inc-amount (if (eql time-quanta :week)
                         (list 7 :day)
                         (list 1 time-quanta))))
    (loop for x = start-date then (apply #'local-time:timestamp+ x inc-amount)
          until (local-time:timestamp>= x end-date) collect (local-time:timestamp-to-universal x))))

(defmethod (setf time-quanta) :after (new-value (self ledger-bar-graph))
  (dolist (dataset (datasets self))
    (setf (time-quanta dataset) (time-quanta self)))
  (setf (x-format-timestring self) (getf *format-timestring-for-quanta* new-value)))

(defmethod x-tick-spacing ((self ledger-bar-graph))
  (with-slots (x-auto-tick x-min x-max x-dtick) self
    (if x-auto-tick (compute-time-ticks self x-min x-max) x-dtick)))

(defmethod display :after ((graph ledger-bar-graph)  stream)
  (setf (stream-cursor-position stream)
        (values (+ (/ (width graph) 2)(left graph))
                (+ 100 (top graph) (height graph))))
  (with-output-as-gadget (stream)
    (let* ((time-quanta (time-quanta graph))
           (time-window (getf '(:month :year :week :month :day :month) time-quanta))
           (prev (make-pane 'push-button
                            :label "<"
                            :active nil
                            :activate-callback
                            (lambda (gadget)
                              (declare (ignore gadget))
                              (setf (start-date graph) (timestamp-quantum- (start-date graph) time-window)
                                    (end-date graph) (timestamp-quantum- (end-date graph) time-window))
                              (setf (auto-scale-needed graph) t)
                              (gr:refresh graph stream))))
           (next (make-pane 'push-button
                            :label ">"
                            :active nil
                            :activate-callback
                            (lambda (gadget)
                              (declare (ignore gadget))
                              (setf (start-date graph) (timestamp-quantum+ (start-date graph) time-window)
                                    (end-date graph) (timestamp-quantum+ (end-date graph) time-window))
                              (setf (auto-scale-needed graph) t)
                              (refresh graph stream)))))
      (if (eql (time-quanta graph) :year)
          (progn (deactivate-gadget prev) (deactivate-gadget next))
          (progn (activate-gadget prev) (activate-gadget next)))
      (horizontally ()
        prev
        (make-pane 'option-pane
                   :items '(:year :month :week :day)
                   :value (time-quanta graph)
                   :value-changed-callback
                   (lambda (value-gadget value)
                     (declare (ignore value-gadget))
                     (setf (time-quanta graph) value)
                     (set-time-window graph)
                     (refresh graph stream)))
        next))))

(defun set-time-window (graph)
  "Auto set START-DATE and END-DATE according to TIME-QUANTA"
  (let* ((end-date (or (and (end-date graph)
                            (local-time:timestamp<= (end-date graph) (local-time:today))
                            (end-date graph))
                       (local-time:today)))
         (quantum (time-quanta graph)))
    (case quantum
      (:year (setf (end-date graph) nil
                   (start-date graph) nil))
      (:month (setf (end-date graph) (timestamp-quantization-inc end-date :year)
                    (start-date graph) (timestamp-quantization-dec end-date :year)))
      (:day (setf (end-date graph) (timestamp-quantization-inc end-date :month)
                  (start-date graph) (timestamp-quantization-dec end-date :month)))
      (:week (setf (end-date graph) (timestamp-quantization-inc end-date :month)
                   (start-date graph) (timestamp-quantization-dec end-date :month)))
      (t t)))
  (setf (auto-scale-needed graph) t))

;;;; LEDGER PIE GRAPH

(defclass ledger-pie-graph (pie-graph ledger-graph)
  ())

(defmethod initialize-instance :after ((instance ledger-pie-graph) &rest initargs)
  (setf (datasets instance)
        (sort (map 'list (lambda (x)
                           (make-instance 'ledger-account-dataset
                                          :account x))
                   (pacioli-model:children (first (graph-accounts instance))))
              (lambda (x y) (> (abs (dataset-summary-function instance x)) (abs (dataset-summary-function instance y)))))))

;;;; LEDGER TIME GRAPH

(defclass ledger-time-graph (x-timestamp-graph-mixin ledger-graph)
  ()
  (:default-initargs :x-tick-numbering :each
                     :x-format-timestring '((:year 4) "-" (:month 2) "-" (:day 2))))

(export '(ledger-account-dataset ledger-time-graph ledger-pie-graph ledger-bar-graph graph-accounts))

;;;;;;; step-window-mixin (bar and pie)
