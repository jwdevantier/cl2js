;;;; cl2js.asd

(asdf:defsystem #:cl2js
  :description "(Semi-)toy ~lisp to javascript compiler"
  :author "Jesper Devantier <jesper.devantier@protonmail.com>"
  :license  "GPL-3.0-only"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :in-order-to ((asdf:test-op (asdf:test-op "cl2js/tests")))
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "ast")
                 (:file "emitter")
                 (:file "emitjs")))))

(asdf:defsystem #:cl2js/tests
  :version "0.1.0"
  :author "Jesper Devantier"
  :license "GPL-3.0-only"
  :depends-on ("cl2js" "rove" "alexandria")
  :description "tests for cl2js"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c))
  :components ((:module "tests"
                :components ((:file "ast")))))
