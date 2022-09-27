(in-package :pacioli)

(define-command (com-start-clobber :name t :command-table cmd-file) ((file 'pathname))
  (pm::start-clobber file)
  (setf (current-journal *application-frame*) pm::*journal*))

(define-command (com-stop-clobber :name t :command-table cmd-file) ()
  (pm::stop-clobber)
  (setf (current-journal *application-frame*) (make-instance 'journal :name "andrea")))

(define-command (com-import-ledger :name t :keystroke (#\O :control) :command-table cmd-file)
    ((file 'pathname))
  (pm:import-ledger file)
  (setf (current-journal *application-frame*) pm::*journal*))
