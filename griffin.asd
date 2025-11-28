(defsystem "griffin"
  :description "Hydrophilic-Lipophilic Balance/Deviation Calculator."
  :author "Erik P Almaraz"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on 
  ("iterate"
   "bordeaux-threads"
   "lparallel"
   "cl-ppcre"
   "cl-dbi"
   ;; UI/X
   "cl-gtk4"
   "cl-gtk4.adw"
   "cl-gdk4")
  :components 
  ((:module "src"
    :components
    ((:module "core"
      :components
      ((:file "hlb")
       (:file "database")))
     (:module "ui"
      :components
      ((:file "widgets")
       (:file "layouts")
       (:file "controller")
       (:file "builder")
       (:file "app")))
     (:file "griffin"  :depends-on ("core" "ui")))))
  :in-order-to ((test-op (test-op "griffin/tests")))
  :long-description "Advanced & Extensible Hydrophilic-Lipophilic Balance/Deviation
Calculator Tool for Surfactant Systems.")

;;; ==============================================================================
;;; Register Systems
;;; ==============================================================================
;; The function `register-system-packages' must be called to register packages
;; used or provided by your system when the name of the system/file that 
;; provides the package is not the same as the package name
;; (converted to lower case).
(register-system-packages "iterate" '(:iter))
(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
(register-system-packages "fiveam" '(:5am))

;;; ==============================================================================
;;; Secondary Systems
;;; ==============================================================================
(defsystem "griffin/tests"
  :depends-on ("griffin" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "suite"))))
  :perform (test-op (o c) 
                    (symbol-call :fiveam :run! :suite)))

(defsystem "griffin/docs"
  :description "Documentation framework"
  :depends-on ("griffin"
               "3bmd"
               "colorize"
               "print-licenses")
  :components
  ((:module "docs"
    :components
    ((:file "griffin-docs")))))

(defsystem "griffin/executable"
  :description "Build executable"
  :depends-on ("griffin")
  :build-operation "program-op"
  :build-pathname "griffin-preexe"
  :entry-point "griffin:main")
