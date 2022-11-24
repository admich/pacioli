;;;; pacioli.asd

(asdf:defsystem #:pacioli
  :description "Accounting software with CLIM interface"
  :author "Andrea De Michele (admich)"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :depends-on (#:mcclim #:cl-ledger #:local-time  #:scigraph-evo/contrib #:adm-clim-lib #:mcclim-completion-patch :clobber)
  :serial t
  :components ((:file "patch")
               (:file "package")
	           (:file "model")
               (:file "model-clobber")
	           (:file "import-export")
               (:file "config")
               (:file "scigraph-ledger")
               (:file "presentations")
               (:file "pacioli"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "pacioli"
  :entry-point "pacioli:pacioli")

