(defpackage #:cl2js/tests/ast
  (:use
   :cl
   :cl2js/ast
   :rove))

(in-package #:cl2js/tests/ast)

;; to test, run (asdf:test-system "cl2js")

(deftest ensure-return
  (testing "NO-OP, 1 expr, :return-form"
    (ok (equalp '((:return :x))
                (ensure-return '((:return :x))))))
  (testing "NO-OP, 2 exprs (last is :return form)"
    (ok (equalp '(:x (:return :y))
                (ensure-return '(:x (:return :y))))))
  (testing "1 expr -> wrap in :return"
    (ok (equalp '((:return :x))
                (ensure-return '(:x)))))
  (testing "2 exprs -> wrap last"
    (ok (equalp '(:x (:return :y))
                (ensure-return '(:x :y)))))
  (testing "require list of inputs"
    (ok (signals (ensure-return :x)))))

(deftest rewrite-ast-noop
  (testing "simplest program"
    (ok (equalp '(:progn :x) (rewrite-ast '(progn x) '()))))
  (testing "nested program"
    (ok (equalp '(:progn
                  (:defun :identity (:x) :x)
                    (:defun :add1 (:x) (:+ 1 :x)))
                  (rewrite-ast '(progn (defun identity (x) x)
                                       (defun add1 (x) (+ 1 x))) '())))))

(deftest rewrite-let-bindings
  (testing "let-binding to transform to IIFE"
    (ok (equalp '(:progn ((:lambda (:x :y)
                            (:print :x)
                            (:print :y)
                            (:return :y)) 1 (:max 10 (:- 1 2))))
                (rewrite-ast '(:progn (:let ((:x 1)
                                             (:y (:max 10 (- 1 2))))
                                        (:print :x)
                                        (:print :y)
                                        :y))
                             '(:let rr/let))))))

;; could rewrite if to cond...
(deftest rewrite-if
  (testing "if-else -> IIFE"
    (ok (equalp '((:lambda () (://if (:> :x 10)
                                     (:progn (:return :x))
                                     (:progn (:return 10)))))
                (rewrite-ast '(if (> x 10) x 10)
                             '(:if rr/if)))))
  (testing "if (one-armed if) -> IIFE"
    (ok (equalp '((:lambda () (://if (:> :x 10)
                                     (:progn (:return :x))
                                     (:progn (:return :nil)))))
                (rewrite-ast '(if (> x 10) x)
                             '(:if rr/if))))))

(deftest rewrite-cond
  (testing "one-armed cond - nil-clause"
    (ok (equalp '((:lambda ()
                    (://cond ((:> :x 10) (:return :x))
                             (:t (:return :nil)))))
                (rewrite-ast '(cond ((> x 10) x))
                             '(:cond rr/cond)))))
  (testing "t-clause given, no additional clauses"
    (ok (equalp '((:lambda ()
                    (://cond ((:> :x 10) (:return :x))
                             (:t (:return 1000)))))
                (rewrite-ast '(cond ((> x 10) x)
                               (t 1000))
                             '(:cond rr/cond)))))
  (testing "multiple clauses"
    (ok (equalp '((:lambda ()
                    (://cond ((:> :x 1000)
                              (:print "x is over 1000")
                              (:return "x is 1000+"))
                             ((:> :x 100)
                              (:print "x is over 100")
                              (:return "x is 100+"))
                             (:t (:return "x is small")))))
                (rewrite-ast '(cond
                               ((> x 1000)
                                (print "x is over 1000")
                                "x is 1000+")
                               ((> x 100)
                                (print "x is over 100")
                                (return "x is 100+"))
                               (t "x is small"))
                             '(:cond rr/cond))))))

;; :let, :test, :iter
(deftest rewrite-for)

(deftest rewrite-defun
  (testing "0-arg fn"
    (ok (equalp '(://defun :greeting (:lambda () "greetings"))
                (rewrite-ast '(defun greeting () "greetings") '(:defun rr/defun)))))
  (testing "1-arg fn"
    (ok (equalp '(://defun :identity (:lambda (:x) :x))
                (rewrite-ast '(defun identity (x) x) '(:defun rr/defun)))))
  (testing "2-arg fn"
    (ok (equalp '(://defun :sum2 (:lambda (:x :y) (:+ :x :y)))
                (rewrite-ast '(defun sum2 (x y) (+ x y)) '(:defun rr/defun))))))

;; TODO - test empty body -- should return undefined
(deftest rewrite-lambda
  (testing "1 body stmt, already return -> noop"
    (ok (equalp '(:lambda (:x) (:return :x))
                (rewrite-ast '(lambda (x) (return x)) '(:lambda rr/lambda)))))
  (testing "n body statements, already return -> noop"
    (ok (equalp '(:lambda (:x)
                  (:print "one")
                  (:print "two")
                  (:return :x))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               (print "two")
                               (return x)) '(:lambda rr/lambda)))))
  (testing "1 body stmt (atom) -> wrap with return"
    (ok (equalp '(:lambda (:x) (:return :x))
                (rewrite-ast '(lambda (x) x) '(:lambda rr/lambda)))))
  (testing "1 body stmt (sexp) -> wrap with return"
    (ok (equalp '(:lambda (:x) (:return (:sum 1 2)))
                (rewrite-ast '(lambda (x) (sum 1 2)) '(:lambda rr/lambda)))))
  (testing "n body stmts, atom -> wrap with return"
    (ok (equalp '(:lambda (:x)
                  (:print "one")
                  (:return :x))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               x) '(:lambda rr/lambda)))))
  (testing "n body stmts, sexp -> wrap with return"
    (ok (equalp '(:lambda (:x)
                  (:print "one")
                  (:return (:sum 1 2)))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               (sum 1 2)) '(:lambda rr/lambda))))))

;; TODO binop
;; TODO unary/pre
;; TODO unary/pre/alias
;; TODO unary/post/alias

;; TODO -- expand, need to test out //prop[] also.
(deftest rewrite-prop-access
  (testing "foo.bar"
    (ok (equalp '(://prop :foo :bar)
                (rewrite-ast '(@ foo bar) '(:@ rr/prop-access))))
    (ok (equalp '(://prop :foo :bar :baz)
                (rewrite-ast '(@ foo bar baz)
                             '(:@ rr/prop-access))))
    (ok (equalp '(://prop (://prop[] :foo "bar") :baz)
                (rewrite-ast '(@ foo "bar" baz)
                             '(:@ rr/prop-access))))
    (ok (equalp '(://prop[] (://prop[] :foo "bar") "baz")
                (rewrite-ast '(@ foo "bar" "baz")
                             '(:@ rr/prop-access))))))

(deftest rewrite-multipass
  (testing "empty rulesets -> NO-OP"
    (ok (equalp '(:progn :x)
                (rewrite-ast-multipass '(progn x) '() '()))))
  (testing "desugar defun, then ensure return last stmt in lambda"
    (ok (equalp '(:progn (://defun :identity (:lambda (:x) (:return :x))))
                (rewrite-ast-multipass
                 '(progn (defun identity (x) x))
                 '(:defun rr/defun) '(:lambda rr/lambda))))))
