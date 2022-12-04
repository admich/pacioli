;;;; pacioli.lisp

(in-package #:pacioli)

(defparameter *scheduled-transactions* nil)

;;; cmd-file command table
(define-command-table cmd-file
  :menu (("Load" :command com-load)
         ("Save" :command com-save)
         ("Import Ledger" :command com-import-ledger)
         ("Export as Ledger" :command com-export-as-ledger)
         ("Read Prices" :command com-read-proces)
         ("Demo" :command com-demo)
         (nil :divider nil)
         ("Quit" :command com-quit)))

(define-command (com-open :name t :keystroke (#\o :control) :command-table cmd-file)
    ((file 'pathname)
     &key
     (name 'string :default "ledger"))
  (setf *scheduled-transactions* nil)
  (pm::start-clobber file name)
  (setf (current-journal *application-frame*) pm::*journal*))

(define-command (com-close :name t :command-table cmd-file) ()
  (pm::stop-clobber)
  (setf (current-journal *application-frame*) (make-instance 'journal :name "None")))

(define-command (com-import-ledger :name t  :command-table cmd-file) ((file 'pathname))
  (pm:import-ledger file))

(define-command (com-export-as-ledger :name t :keystroke (#\E :control) :command-table cmd-file) ((file 'pathname))
  (export-ledger (current-journal *application-frame*) file))

(define-command (com-read-prices :name t :command-table cmd-file) ((file 'pathname))
  (import-ledger-prices file))

(define-command (com-demo :name t :command-table cmd-file) ()
  (pm::stop-clobber)
  (let ((journal (make-instance 'journal :name "Demo")))
    (pm:execute 'pm::init-clobber-journal journal))
  (pm:import-ledger
         (merge-pathnames "data/demo.ledger" (asdf:system-source-directory :pacioli)))
  (setf (current-journal *application-frame*) pm::*journal*))

(define-command (com-quit :name t :keystroke (#\q :control) :command-table cmd-file) ()
  (frame-exit *application-frame*))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter pacioli-menu
      '(("File" :menu cmd-file)
        ("Go-Back" :command com-go-back-view :keystroke (#\B :control))
        ("Go-Forward" :command com-go-forward-view :keystroke (#\F :control)))))

(defclass pacioli-frame-virtual-account (virtual-account)
  ((%frame :initarg :frame :initform nil :accessor frame)))

(defmethod transactions-sink ((account pacioli-frame-virtual-account))
  (a:when-let ((parent (parent account)))
    (transactions (current-journal (frame account)))))

(defmethod parent :around ((account pacioli-frame-virtual-account))
  (or (call-next-method) (current-journal (frame account))))

(defun make-broken-transactions-vaccount (frame)
  (make-instance 'pacioli-frame-virtual-account
                 :frame frame
                 :name "Broken transactions"
                 :fn-transaction-p #'transaction-broken-p
                 :fn-amount-for-account #'total-amount))
;;;; application definition
(define-application-frame pacioli (acl:store-history-application-mixin)
  ((%current-journal :initform (make-instance 'journal :name "andrea") :accessor current-journal)
   (%reconcile-account :initform nil :accessor reconcile-account)
   (%reconcile-target :initform 0 :accessor reconcile-target)
   (%reconcile-current-entry :initform nil :accessor reconcile-current-entry)
   (%reconcile-current-entry-presentation :initform nil :accessor reconcile-current-entry-presentation)
   (%virtual-accounts :initform '() :accessor virtual-accounts))
  (:command-table (pacioli :inherit-from (cmd-file acl:treeview acl:zelig :graph)
                           :menu #.pacioli-menu))
  (:pointer-documentation t)
  (:panes
   (inter :interactor
          :height 100)
   (main (make-pane 'acl:application-with-display-history-pane
                    :display-function 'acl:display-pane
                    :incremental-redisplay nil
                    :display-time :command-loop))
   (secondary :application
              :display-function 'acl:display-pane
              :incremental-redisplay nil
              :display-time :command-loop
              :scroll-bars :both)
   (reconcile-status :application
              :display-function 'display-reconcile-status
              :incremental-redisplay nil
              :background +lightyellow2+
              :height 22
              :display-time :command-loop
              :scroll-bars nil)
   (accounts-tree
    (make-pane 'acl:treeview-pane
               :tree-roots #'(lambda (frame) (list (current-journal frame)))
               :printer
               #'(lambda (node s)
                   (with-output-as-presentation (s node 'account)
                     (format s "~a" (name node))))
               :tabular-printers
               (list (list #'(lambda (node s) (present (balance node) 'amount :stream s))
                           :align-x :right)
                     (list #'(lambda (node s) (present (balance node) '((amount) :value :market) :stream s))
                           :align-x :right))
               :inferior-producer
               #'(lambda (x) (loop for i in (pacioli-model::children x) collect i))
               :display-function 'acl:display-pane
               :incremental-redisplay t
               :default-view acl:+tabular-view+))
   (vaccounts-tree
    (make-pane 'acl:treeview-pane
               :tree-roots #'(lambda (frame)
                               (virtual-accounts frame))
               :printer
               #'(lambda (node s)
                   (with-output-as-presentation (s node 'virtual-account)
                     (format s "~a" (name node))))
               :tabular-printers
               (list
                (list #'(lambda (node s) (format s "(~d)" (length (transactions node))))
                           :align-x :right)
                (list #'(lambda (node s) (present (balance node) 'amount :stream s))
                           :align-x :right)
                (list #'(lambda (node s) (present (balance node) '((amount) :value :market) :stream s))
                           :align-x :right))
               :inferior-producer
               #'(lambda (x) (loop for i in (pacioli-model::children x) collect i))
               :display-function 'acl:display-pane
               :incremental-redisplay t
               :default-view acl:+tabular-view+)))
  (:layouts (default
             (horizontally (:height (graft-height (find-graft))
                            :width (graft-width (find-graft)))
               (1/4 (vertically ()
                      (2/3 (scrolling (:scroll-bars t) accounts-tree))
                      (1/3 (scrolling (:scroll-bars t) vaccounts-tree))))
               (make-pane 'clim-extensions:box-adjuster-gadget)
               (3/4 (vertically ()
                      (1 secondary)
                      (:fill (scrolling (:scroll-bars t) main))
                      (make-pane 'clim-extensions:box-adjuster-gadget)
                      inter))))
            (reconcile
                (horizontally (:height (graft-height (find-graft))
                               :width (graft-width (find-graft)))
                  (1/4 (vertically ()
                         (2/3 (scrolling (:scroll-bars t) accounts-tree))
                         (1/3 (scrolling (:scroll-bars t) vaccounts-tree))))
                  (make-pane 'clim-extensions:box-adjuster-gadget)
                  (3/4 (vertically ()
                         reconcile-status
                         (1/2 secondary)
                         (make-pane 'clim-extensions:box-adjuster-gadget)
                         (:fill (scrolling (:scroll-bars t) main))
                         (make-pane 'clim-extensions:box-adjuster-gadget)
                         inter)))))
  (:reinitialize-frames t)
  (:default-initargs :history-file (merge-pathnames "history.store" *config-directory*)))

(defmethod run-frame-top-level :before ((frame pacioli) &key)
  (push (make-broken-transactions-vaccount frame) (virtual-accounts frame)))

(defun pacioli (&key new-process (restore-history t))
  (if new-process
      (clim-sys:make-process (lambda () (run-frame-top-level (make-application-frame 'pacioli :restore-history-p restore-history))) :name "pacioli")
      (run-frame-top-level (make-application-frame 'pacioli))))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view textual-view))
  (format pane "Pacioli"))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view view-account))
  (let ((account (view-account view)))
    (format pane "~a~%" (long-name account))
    (present (balance account) '((amount) :value :all) :stream pane)
    (terpri pane)
    (formatting-table (pane :y-spacing 5 :x-spacing '(2 :character))
      (loop for i in (transactions (view-account view))
         for background = t then (not background) do
           (if background
               (surrounding-output-with-border (pane :background +lightgrey+ :line-thickness 0 :ink +transparent-ink+)
                 (present i 'transaction :view acl:+tabular-view+))
               (present i 'transaction :view acl:+tabular-view+))))))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view view-commodities))
  (formatting-table (pane)
    (formatting-row (pane)
      (formatting-cell (pane)
        (format pane "Valuta"))
      (formatting-cell (pane)
        (format pane "Prezzo attuale"))
      (formatting-cell (pane)
        (format pane "numero di prezzi")))
    (dolist (x (journal-commodities (current-journal *application-frame*)))
      (formatting-row (pane)
        (formatting-cell (pane)
          (present x 'commodity :stream pane))
        (formatting-cell (pane)
          (present (last-price x) 'price :stream pane))
        (formatting-cell (pane)
          (format pane "~a" (number-of-prices x)))))))


(define-pacioli-command (com-go-back-view :name t) ()
  (acl:undo-display-history (find-pane-named *application-frame* 'main)))

(define-pacioli-command (com-go-forward-view :name t) ()
  (acl:redo-display-history (find-pane-named *application-frame* 'main)))

(define-gesture-name :scroll-down :keyboard (#\v :control) :unique t)
(define-gesture-name :scroll-up :keyboard (#\v :meta) :unique t)
(define-gesture-name :scroll-to-top :keyboard (#\< :control) :unique t)
(define-gesture-name :scroll-to-bottom :keyboard (#\> :control) :unique t)

(add-keystroke-to-command-table 'pacioli :scroll-down
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (let* ((pane (frame-standard-output *application-frame*))
                         (viewport-region (pane-viewport-region pane))
                         (viewport-height (rectangle-height viewport-region)))
                    (multiple-value-bind (x y) (window-viewport-position pane)
                      (scroll-extent pane x (+ y viewport-height))))
                  nil)
     :documentation "Scroll the main pane down")

(add-keystroke-to-command-table 'pacioli :scroll-up
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (let* ((pane (frame-standard-output *application-frame*))
                         (viewport-region (pane-viewport-region pane))
                         (viewport-height (rectangle-height viewport-region)))
                    (multiple-value-bind (x y) (window-viewport-position pane)
                      (scroll-extent pane x (- y viewport-height))))
                  nil)
      :documentation "Scroll the main pane up")

(add-keystroke-to-command-table 'pacioli :scroll-to-top
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (let ((pane (frame-standard-output *application-frame*)))
                      (scroll-extent pane 0 0))
                  nil)
      :documentation "Scroll the main pane to the top")

(add-keystroke-to-command-table 'pacioli :scroll-to-bottom
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (let* ((pane (frame-standard-output *application-frame*))
                         (height (bounding-rectangle-height (sheet-region pane)))
                         (viewport-region (pane-viewport-region pane))
                         (viewport-height (rectangle-height viewport-region)))
                    (scroll-extent pane 0 (- height viewport-height))
                    nil))
      :documentation "Scroll the main pane to the top")

(defun set-main-view (view)
  (let ((command-table (frame-command-table *application-frame*)))
      (map-over-command-table-commands
       (lambda (cmd)
         (when (eq (find-command-table :graph)
                   (command-accessible-in-command-table-p
                           cmd
                           command-table))
           (setf (command-enabled (command-name cmd) *application-frame*)
                 (typep view 'view-plot))))
       command-table))
  (setf (stream-default-view (find-pane-named *application-frame* 'main)) view))

(defun set-secondary-view (view)
  (setf (stream-default-view (find-pane-named *application-frame* 'secondary)) view))

(define-pacioli-command (com-refresh :name t) ()
  (redisplay-frame-panes *application-frame*))

(define-pacioli-command (com-view-account :name t) ((account 'general-account :gesture :select))
  (set-main-view (make-instance 'view-account :account account)))

(define-pacioli-command (com-view-commodities :name t) ()
  (set-main-view (make-instance 'view-commodities)))


;;;; Plot commands
(defmethod acl:display-pane-with-view ((frame pacioli) pane (view view-plot))
  (let ((graph (view-plot-graph view)))
    ;; when use refresh instead of display-graph?
    (gr:display-graph graph :stream pane :width 800 :height 600)))

(define-pacioli-command (com-two-accounts-bar-plot :name t)
    ((account1 'account) (account2 'account))
  (let ((graph (make-instance 'gr:ledger-bar-graph
                              :accounts (list account1 account2
                                              (make-join-account account1 account2)))))
    (set-main-view (make-instance 'view-plot :graph graph))))

(define-pacioli-command (com-plot-account :name t) ((account 'account))
  (let ((graph (make-instance 'gr:ledger-time-graph
                              :title (format nil "~a" (long-name account))
                              :show-legend nil)))
    (gr:add-dataset graph (make-instance 'gr:ledger-account-dataset :account account :symbologies (list :step)))
    (set-main-view (make-instance 'view-plot :graph graph))))

(define-presentation-to-command-translator
  com-plot-account
    (account  com-plot-account pacioli
	 :documentation "Plot Account"
	 :gesture nil)
  (object)
  (list object))

(define-pacioli-command (com-pie-account :name t) ((account 'account))
  (let ((graph (make-instance 'gr:ledger-pie-graph
                              :title (format nil "~a" (long-name account))
                              :accounts (list account))))
    (set-main-view (make-instance 'view-plot :graph graph))))

(define-presentation-to-command-translator
  com-pie-account
    (account  com-pie-account pacioli
	 :documentation "Pie Account"
	 :gesture nil)
  (object)
  (list object))

(define-pacioli-command (com-bar-account :name t) ((account 'account))
  (let ((graph (make-instance 'gr:ledger-bar-graph
                              :title (format nil "~a" (long-name account))
                              :accounts (list account))))
    (set-main-view (make-instance 'view-plot :graph graph))))

(define-presentation-to-command-translator
  com-bar-account
    (account  com-bar-account pacioli
	 :documentation "Bar Account"
	 :gesture nil)
  (object)
  (list object))


;;;; Edit command
(define-pacioli-command (com-new-account :name t)
    ((name 'string) (parent 'account :default (current-journal *application-frame*)))
  (let ((account (make-instance 'account :name name :parent parent)))
    (pm:execute 'pm:register-account account)))

(define-pacioli-command (com-rename-account :name t) ((account 'account) (name 'string))
  (pm:execute 'pm:rename-account account name))

(define-pacioli-command (com-edit-transaction :name t)
    ((transaction 'transaction
      :gesture (:select
                :tester ((transaction)
                         (let ((view (stream-default-view (find-pane-named *application-frame* 'main))))
                           (or (not (typep view 'edit-transaction-view))
                               (not (eql transaction (view-transaction view)))))))))
      (set-main-view
            (make-instance 'edit-transaction-view :transaction transaction)))

(define-presentation-to-command-translator edit-entry
    (entry com-edit-transaction pacioli :gesture :edit)
    (entry)
  (list (transaction entry)))

(define-pacioli-command (com-new-transaction :name t :keystroke (#\a :meta))
    ((date 'timestamp) (name 'string) (account1 'account) (value 'value) (account2 'account))
  (let* ((amount1 (make-instance 'single-commodity-amount
                                 :value value :commodity *main-commodity*))
         (amount2 (make-instance 'single-commodity-amount
                                 :value (- value) :commodity *main-commodity*))
         (transaction (make-instance 'transaction :date date :name name))
         (entry1 (make-instance 'entry :account account1 :amount amount1))
         (entry2 (make-instance 'entry :account account2 :amount amount2)))
    (pm:execute 'pm:register-entries* transaction entry1 entry2)
    (pm:execute 'pm:register-transaction (current-journal *application-frame*) transaction)
    (set-main-view
     (make-instance 'edit-transaction-view :transaction transaction))))

(define-pacioli-command (com-clone-transaction :name t :keystroke (#\A :meta))
    ((date 'timestamp) (xact 'transaction) (value '(null-or-type value)))
  (let* ((transaction (make-instance 'transaction :date date :name (name xact) :tags (tags xact))))
    (dolist (entry (entries xact))
      (pm:execute 'pm:register-entries* transaction (clone-entry entry value)))
    (pm:execute 'pm:register-transaction (current-journal *application-frame*) transaction)
    (set-main-view
          (make-instance 'edit-transaction-view :transaction transaction))))

(define-pacioli-command (com-delete-transaction :name t)
    ((transaction 'transaction :gesture :delete))
  (pm:execute 'pm:delete-transaction transaction (current-journal *application-frame*)))

(define-pacioli-command (com-delete-entry :name t)
    ((entry 'entry :gesture :delete))
  (pm:execute 'pm:delete-entry entry))

(define-pacioli-command (com-add-entry-to-transaction :name t)
    ((transaction 'transaction) (account 'account) (value 'value))
  (let ((entry (make-instance 'entry :account account
                         :amount (make-instance 'single-commodity-amount
                                                :value value :commodity *main-commodity*))))
  (pm:execute 'pm:register-entries* transaction entry)))

(defclass edit-transaction-view (textual-view)
  ((%transaction :initarg :transaction :reader view-transaction)))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view edit-transaction-view))
  (let* ((*standard-output* pane)
         (transaction (view-transaction view))
         (entries (entries transaction)))
    (if (find transaction (transactions (current-journal *application-frame*)))
        (with-output-as-gadget (pane)
          (make-pane 'push-button
                 :label "Delete Transaction"
                 :activate-callback
                 (lambda (gadget)
                   (declare (ignore gadget))
                   (execute-frame-command *application-frame*
                                          `(com-delete-transaction ,transaction)))))
        (progn (format t "The transaction isn't registered in journal.~2%")
               (with-output-as-gadget (pane)
                 (make-pane 'push-button
                            :label "Register Transaction"
                            :activate-callback
                            (lambda (gadget)
                              (declare (ignore gadget))
                              (pm:execute 'pm:register-transaction
                                          (current-journal *application-frame*)
                                          transaction)
                              (execute-frame-command *application-frame*
                                                     '(com-refresh)))))))
    (fresh-line)
    (with-output-as-presentation (t transaction 'transaction :single-box t)
      (present transaction 'transaction-date)
      (write-string "  ")
      (present transaction 'transaction-name)
      (fresh-line)
      (write-string "Note: ")
      (present transaction 'transaction-note)
      (fresh-line)
      (write-string "Tags: ")
      (loop :for tag :in (tags transaction) :do
        (present (list transaction tag) 'tag-of-object)
        (write-string " "))
      (present (list transaction nil) 'tag-of-object)
      (fresh-line)
      (formatting-table (t :x-spacing '(2 :character))
        (loop :for entry :in entries
              :for am = (amount entry) do
                (with-output-as-presentation (t entry 'entry :single-box t)
                  (formatting-row ()
                    (formatting-cell () (present entry 'entry-reconciled))
                    (formatting-cell () (present entry 'entry-account))
                    (formatting-cell () (present am 'single-commodity-amount-value))
                    (formatting-cell () (present am 'single-commodity-amount-commodity))
                    (formatting-cell ()
                      (write-string "Note: ")
                      (present entry 'entry-note))
                    (formatting-cell ()
                      (write-string "Tags: ")
                      (loop :for tag :in (tags entry) :do
                        (present (list entry tag) 'tag-of-object)
                        (write-string " "))
                      (present (list entry nil) 'tag-of-object)))))))
    (fresh-line)
    (with-output-as-gadget (pane)
      (make-pane 'push-button
                 :label "Add Entry"
                 :activate-callback
                 (lambda (gadget)
                   (declare (ignore gadget))
                   (execute-frame-command *application-frame*
                                          `(com-add-entry-to-transaction ,transaction)))))))

(defmacro define-presentation-editor (class slot)
  (let* ((slot-accessor (first slot))
         (presentation-name (a:symbolicate class "-" slot-accessor))
         (command-name (a:symbolicate "COM-EDIT-" presentation-name))
         (slot-ptype (second slot)))
      `(progn
         (define-presentation-type ,presentation-name () :inherit-from ',class)
         (define-presentation-method present (object (type ,presentation-name) stream view &key)
           (present (,slot-accessor object) ',slot-ptype :stream stream :sensitive nil))
         (define-pacioli-command (,command-name :name t)
             ((object ,presentation-name :gesture :edit))
           (let ((new-value (accept ',slot-ptype :default (,slot-accessor object) :insert-default t)))
             (pm:execute 'pm:modify-object object ',slot-accessor new-value))))))

(define-presentation-editor transaction (date timestamp))
(define-presentation-editor transaction  (name string))
(define-presentation-editor transaction  (note (null-or-type string)))
(define-presentation-editor entry   (account account))
(define-presentation-editor entry   (note (null-or-type string)))
(define-presentation-editor entry   (reconciled reconcile-status))
(define-presentation-editor single-commodity-amount   (value value))
(define-presentation-editor single-commodity-amount   (commodity commodity))

;; tags
(define-presentation-type tag ())

(define-presentation-method presentation-typep (object (type tag))
  (typep object 'keyword))

(define-presentation-method present (object (type tag) stream (view textual-view) &key)
  (declare (ignore view))
  (format stream "#~a" object))

(define-presentation-method accept ((type tag) stream (view textual-view) &key)
  (declare (ignore view))
  (multiple-value-bind (obj succ str)
      (completing-from-suggestions (stream :allow-any-input t)
               (dolist (tag *possible-tags*)
                 (suggest (format nil "~a" tag) tag)))
    (unless obj
      (setf obj (a:make-keyword (string-upcase str)))
      (pushnew obj *possible-tags*))
    obj))

(define-presentation-type tag-of-object ())

(define-presentation-method presentation-typep (object (type tag-of-object))
  (and
   (listp object)
   (or (null (second object)) (typep (second object) 'keyword))
   (typep (first object) 'pacioli-model::tags-mixin)))

(define-presentation-method present (object (type tag-of-object) stream (view textual-view) &key)
  (declare (ignore view))
  (a:if-let ((tag (second object)))
    (present (second object) 'tag)
    (format stream "Add tag")))

(define-pacioli-command  (com-set-tags :name t)
    ((object-with-tags 'pacioli-model::tags-mixin) (tags '(sequence tag)))
  (pm:execute 'pm:modify-object object-with-tags 'pm:tags tags))

(define-pacioli-command  (com-delete-tag)
    ((tag 'tag-of-object :gesture :delete))
  (let ((object-with-tag (first tag))
        (tag (second tag)))
    (pm:execute 'pm:delete-tag object-with-tag tag)))

(define-pacioli-command  (com-edit-tag)
    ((tag 'tag-of-object :gesture :edit)
     (new-tag 'tag))
  (let ((object-with-tag (first tag))
        (tag (second tag)))
    (pm:execute 'pm:delete-tag object-with-tag tag)
    (pm:execute 'pm:add-tags object-with-tag (list new-tag))))

(define-pacioli-command  (com-add-tags)
    ((tag 'tag-of-object :gesture :select)
     (new-tags '(sequence tag)))
  (let ((object-with-tag (first tag)))
    (pm:execute 'pm:add-tags object-with-tag new-tags)))


;;;; Reconcile
(define-command-table pacioli-reconcile
  :inherit-from (pacioli)
  :menu #.(append
           pacioli-menu
           '((nil :divider nil) ("Finish Reconcile" :command com-reconcile-quit)
             ;; necessary to inherit the keystrokes of pacioli map
             (nil :menu pacioli))))

(defclass reconcile-view (textual-view)
  ())

(defun entries-for-reconcile (account)
  (loop :for transaction :in (transactions account) :append
    (loop :for entry :in (entries transaction)
          :when (and (eql account (account entry))
                     (member (reconciled entry) '(nil :pending)))
            :collect entry)))

(define-pacioli-command (com-reconcile-account :name t)
    ((account 'account) (value 'value))
  ;; check if account have only eur
  (set-secondary-view (make-instance 'reconcile-view))
  (set-main-view (make-instance 'view-account :account account))
  (setf (frame-command-table *application-frame*) (find-command-table 'pacioli-reconcile))
  (setf (reconcile-account *application-frame*) account
        (reconcile-target *application-frame*) value)
  (setf (frame-current-layout *application-frame*) 'reconcile))

(define-command (com-reconcile-quit :name t :keystroke (#\q :control) :command-table pacioli-reconcile)
    ()
  (let ((account (reconcile-account *application-frame*)))
    (loop :for entry :in (entries-for-reconcile account)
          :when (eql (reconciled entry) :pending) :do
            (pm:execute 'modify-object entry 'reconciled t)))
  (set-secondary-view +textual-view+)
  (setf (frame-command-table *application-frame*) (find-command-table 'pacioli))
  (setf (frame-current-layout *application-frame*) 'default))

(defun go-next-entry ()
  (let ((entries (entries-for-reconcile (reconcile-account *application-frame*)))
        (window (find-pane-named *application-frame* 'secondary)))
    (with-accessors ((centry reconcile-current-entry)) *application-frame*
      (let ((pos (position centry entries)))
        (if pos
            (setf centry (nth (min (1- (length entries))
                                   (1+ pos))
                              entries))
            (setf centry (car entries))))
      (when centry
        (set-main-view (make-instance 'edit-transaction-view
                                      :transaction (transaction centry))))
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main))
      (multiple-value-bind (x y) (window-viewport-position window)
        (redisplay-frame-pane *application-frame* window)
        (multiple-value-bind (cx cy)
            (output-record-position (reconcile-current-entry-presentation *application-frame*))
          (if (> (+ cy (bounding-rectangle-height
                        (reconcile-current-entry-presentation *application-frame*)))
                 (+ y (rectangle-height (pane-viewport-region window))))
              (scroll-extent window 0 (+ (- cy (rectangle-height (pane-viewport-region window)))
                                         (bounding-rectangle-height
                                          (reconcile-current-entry-presentation *application-frame*))
                                         5))
              (scroll-extent window x y)))))))

(defun go-prev-entry ()
  (let ((entries (entries-for-reconcile (reconcile-account *application-frame*)))
        (window (find-pane-named *application-frame* 'secondary)))
    (with-accessors ((centry reconcile-current-entry)) *application-frame*
      (let ((pos (position centry entries)))
        (if pos
            (setf centry (nth (max 0
                                   (1- pos))
                              entries))
            (setf centry (car entries))))
      (when centry
        (set-main-view (make-instance 'edit-transaction-view
                                      :transaction (transaction centry))))
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main))
      (multiple-value-bind (x y) (window-viewport-position window)
        (redisplay-frame-pane *application-frame* window)
        (multiple-value-bind (cx cy)
            (output-record-position (reconcile-current-entry-presentation *application-frame*))
          (if (< cy y)
              (scroll-extent window 0 cy)
              (scroll-extent window x y)))))))

(define-gesture-name pacioli-reconcile-next-entry :keyboard :down)
(define-gesture-name pacioli-reconcile-next-entry :keyboard (#\n :meta) :unique nil)
(define-gesture-name pacioli-reconcile-prev-entry :keyboard :up)
(define-gesture-name pacioli-reconcile-prev-entry :keyboard (#\p :meta) :unique nil)
(define-gesture-name pacioli-reconcile-toogle-entry :keyboard (#\Return :meta))
(define-gesture-name pacioli-reconcile-toogle-entry :keyboard :right :unique nil)

(add-keystroke-to-command-table 'pacioli-reconcile 'pacioli-reconcile-next-entry
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (go-next-entry)
                  nil)
      :documentation "Go to the next entry")

(add-keystroke-to-command-table 'pacioli-reconcile 'pacioli-reconcile-prev-entry
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (go-prev-entry)
                  nil)
      :documentation "Go to the prev entry")

(defun toggle-entry-reconciled (entry window)
    (case (reconciled entry)
      (:pending (pm:execute 'pm:modify-object entry 'pm:reconciled nil))
      ((nil) (pm:execute 'pm:modify-object entry 'pm:reconciled :pending)))
    (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'reconcile-status))
    (multiple-value-bind (x y) (window-viewport-position window)
      (redisplay-frame-pane *application-frame* window)
      (scroll-extent window x y)))

(add-keystroke-to-command-table 'pacioli-reconcile 'pacioli-reconcile-toogle-entry
      :function (lambda (gesture num)
                  (declare (ignore gesture num))
                  (toggle-entry-reconciled (reconcile-current-entry *application-frame*)
                                        (find-pane-named *application-frame* 'secondary))
                  nil)
      :documentation "Toggle reconciled status of the current entry")

(define-presentation-action toggle-entry-reconciled
    (entry nil pacioli-reconcile
           :gesture :select
           :tester ((entry window)
                    (and (null (eq t (reconciled entry)))
                         (typep (stream-default-view window) 'reconcile-view)))
           :documentation "Toggle reconcile status"
           :pointer-documentation "Toggle reconcile status")
    (entry window)
  (toggle-entry-reconciled entry window)
  (setf (reconcile-current-entry *application-frame*) entry))

(defmethod acl:display-pane-with-view ((frame pacioli) pane (view reconcile-view))
  (let ((*standard-output* pane)
        (account (reconcile-account frame)))
    (labels ((present-entry (entry)
               (with-output-as-presentation (t entry 'entry :single-box t)
                   (let ((face (if (null (reconciled entry))
                                   :roman
                                   :bold)))
                     (with-text-face (t face)
                       (formatting-row ()
                     (formatting-cell ()
                       (present (date entry)))
                     (formatting-cell ()
                       (present (name entry)))
                     (formatting-cell ()
                       (present (value (amount entry)) 'value))
                     (formatting-cell ()
                       (present (commodity (amount entry))))))))))
      (formatting-table (t :x-spacing '(3 :character))
        (loop :for entry :in (entries-for-reconcile account) :do
          (if (eql entry (reconcile-current-entry *application-frame*))
              (surrounding-output-with-border
                  (t :shape :underline)
                (setf (reconcile-current-entry-presentation *application-frame*)
                      (present-entry entry)))
              (present-entry entry))))
    (with-output-as-gadget (pane)
      (make-pane 'push-button
                 :label "Done"
                 :activate-callback
                 (lambda (gadget)
                   (declare (ignore gadget))
                   (execute-frame-command *application-frame*
                                          '(com-reconcile-quit))))))))

(defun display-reconcile-status (frame pane)
  (let ((*standard-output* pane)
        (account (reconcile-account frame))
        (target (reconcile-target frame)))
    (with-drawing-options (t :ink +blue+)
      (with-text-face (t :bold)
        (when account
          (formatting-table (t :x-spacing '(4 :character))
            (formatting-row ()
              (formatting-cell ()
                (present account))
              (formatting-cell ()
                (write-string "Balance: ")
                (present (balance account)))
              (formatting-cell ()
                (write-string "Reconciled: ")
                (present (balance account :reconciled t)))
              (formatting-cell ()
                (write-string "Target: ")
                (present target 'value))
              (formatting-cell ()
                (write-string " Difference: ")
                (present (- target (value (convert-amount (balance account :reconciled t)))) 'value)))))))))


;;;; search
(define-pacioli-command (com-search-regexp-in-name :name t :menu nil)
    ((string 'string))
  (push (make-instance 'pacioli-frame-virtual-account
                 :frame *application-frame*
                 :name (format nil "Search ~s" string)
                 :fn-transaction-p (lambda (transaction)
                                     (let ((s (ppcre:create-scanner
                                               string
                                               :case-insensitive-mode t)))
                                       (ppcre:scan s (name transaction))))
                 :fn-amount-for-account #'total-amount)
        (virtual-accounts *application-frame*)))
