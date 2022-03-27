;;;; pacioli.asd

(asdf:defsystem #:pacioli
  :description "Accounting software with CLIM interface"
  :author "Andrea De Michele (admich)"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :depends-on (#:mcclim #:cl-ledger #:local-time  #:scigraph-evo/contrib #:adm-clim-lib #:mcclim-completion-patch)
  :serial t
  :components ((:file "patch")
               (:file "package")
	           (:file "model")
	           (:file "import-export")
               (:file "config")
               (:file "scigraph-ledger")
               (:file "presentations")
               (:file "pacioli")))

