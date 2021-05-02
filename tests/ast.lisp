(defpackage #:cl2js/tests/ast
  (:use
   :cl
   :cl2js
   :rove))

(in-package #:cl2js/tests/ast)

;; to test, run (asdf:test-system "cl2js")

(deftest rewrite-ast-noop
  (testing "simplest program"
    (let ((prog '(progn x)))
      (ok (equalp prog (rewrite-ast prog '())))))
  (testing "nested program"
    (let ((prog '(progn
                  (defun identity (x) x)
                  (defun add1 (x) (+ 1 x)))))
      (ok (equalp prog (rewrite-ast prog '()))))))

;; TODO - test empty body -- should return undefined
(deftest test-rewrite-lambda
  (testing "1 body stmt, already return -> noop"
    (ok (equalp '(lambda (x) (return x))
                (rewrite-ast '(lambda (x) (return x)) '(lambda rr/lambda)))))
  (testing "n body statements, already return -> noop"
    (ok (equalp '(lambda (x)
                  (print "one")
                  (print "two")
                  (return x))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               (print "two")
                               (return x)) '(lambda rr/lambda)))))
  (testing "1 body stmt (atom) -> wrap with return"
    (ok (equalp '(lambda (x) (return x))
                (rewrite-ast '(lambda (x) x) '(lambda rr/lambda)))))
  (testing "1 body stmt (sexp) -> wrap with return"
    (ok (equalp '(lambda (x) (return (sum 1 2)))
                (rewrite-ast '(lambda (x) (sum 1 2)) '(lambda rr/lambda)))))
  (testing "n body stmts, atom -> wrap with return"
    (ok (equalp '(lambda (x)
                  (print "one")
                  (return x))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               x) '(lambda rr/lambda)))))
  (testing "n body stmts, sexp -> wrap with return"
    (ok (equalp '(lambda (x)
                  (print "one")
                  (return (sum 1 2)))
                (rewrite-ast '(lambda (x)
                               (print "one")
                               (sum 1 2)) '(lambda rr/lambda))))))

(deftest test-rewrite-defun
  (testing "0-arg fn"
    (ok (equalp '(let greeting (lambda () "greetings"))
                (rewrite-ast '(defun greeting () "greetings") '(defun rr/defun)))))
  (testing "1-arg fn"
    (ok (equalp '(let identity (lambda (x) x))
                (rewrite-ast '(defun identity (x) x) '(defun rr/defun)))))
  (testing "2-arg fn"
    (ok (equalp '(let sum2 (lambda (x y) (+ x y)))
                (rewrite-ast '(defun sum2 (x y) (+ x y)) '(defun rr/defun))))))

(deftest test-rewrite-multipass
  (testing "empty rulesets -> NO-OP"
    (ok (equalp '(progn x)
                (rewrite-ast-multipass '(progn x) '() '()))))
  (testing "desugar defun, then ensure return last stmt in lambda"
    (ok (equalp '(progn (let identity (lambda (x) (return x))))
                (rewrite-ast-multipass
                 '(progn (defun identity (x) x))
                 '(defun rr/defun) '(lambda rr/lambda))))))
