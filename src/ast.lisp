(defpackage #:cl2js/ast
  (:use #:cl #:cl2js/utils)
  (:local-nicknames (:a :alexandria))
  (:export #:code-printer
           #:rewrite-ast
           #:rewrite-ast-multipass
           #:keywordize-ast
           #:rr/if
           #:rr/defun
           #:rr/lambda
           #:rr/binop
           #:rr/unary/pre
           #:rr/unary/pre/alias
           #:rr/unary/post/alias
           #:rr/prop-access))

(in-package #:cl2js/ast)

(defun rewrite-ast* (ast ruleset)
  "parse AST, applying rewrite ruleset"
  (labels ((rewrite-ast** (ast)
           (cond ((atom ast) ast)
                 ((keywordp (car ast))
                  (let ((rewrite-rule (getf ruleset (car ast))))
                    (if rewrite-rule
                        (funcall rewrite-rule ast #'rewrite-ast**)
                        (mapcar #'rewrite-ast** ast))))
                 (t (mapcar #'rewrite-ast** ast)))))
    (rewrite-ast** ast)))

(defun rewrite-ast (ast ruleset)
  "parse AST, applying rewrite ruleset"
  (rewrite-ast* (keywordize-ast ast) ruleset))

(defun rewrite-ast-multipass (ast &rest rulesets)
  "parse AST, applying each ruleset in turn."
  (reduce #'rewrite-ast rulesets :initial-value ast))

(defun kw-of-string (s)
  "create/get keyword given string"
  (intern (string-upcase s) "KEYWORD"))

;; TODO () => nil.. still OK for emit ?
(defun keywordize-ast (ast)
  (cond ((eq nil ast) ast)
        ((symbolp ast) (kw-of-string ast))
        ((atom ast) ast)
        (t (mapcar #'keywordize-ast ast))))

(defun rr/if (ast parse-fn)
  "ensure if-forms also have an else-branch (remove one-armed if's)"
  (if (eq (length ast) 3)
      (destructuring-bind (kw_ cond then-branch) ast
        (declare (ignore kw_))
        `(:if ,(funcall parse-fn cond)
              ,(funcall parse-fn then-branch) :nil))
      (destructuring-bind (kw_ cond then-branch else-branch) ast
        (declare (ignore kw_))
        `(:if ,(funcall parse-fn cond)
              ,(funcall parse-fn then-branch)
              ,(funcall parse-fn else-branch)))))

(defun rr/defun (ast parse-fn)
  "desugar function declaration into let assignment of a fat-arrow function"
  ;; in (defun foo (...args) &body)
  ;; out (let foo (lambda (...args) &body))
  (destructuring-bind (_ fname args &rest body) ast
    (declare (ignore _))
    `(:let ,fname (:lambda ,args ,@(mapcar parse-fn body)))))

(defun rr/lambda (ast parse-fn)
  "rewrite lambda to return result of last stmt"
  (destructuring-bind (_ args &rest body) ast
    (declare (ignore _))
    (let ((stmts (butlast body 1))
          (last-stmt (car (last body))))
      (if (and (listp last-stmt) (eq (car last-stmt) :return))
          (mapcar parse-fn ast)
          `(:lambda ,args
              ,@(mapcar parse-fn stmts)
              (:return ,(funcall parse-fn last-stmt)))))))

;; rewriting binary- and unary operator calls.
;; This eases complexity in the JS emitter, which is effectively
;; single-pass, therefore taking on the complexity now is preferred.
(defun rr/binop (ast parse-fn)
  "rewrite binary operator expressions to ease JS-emitter"
  (destructuring-bind (op expr1 expr2) ast
    `(://binary ,op ,(funcall parse-fn expr1) ,(funcall parse-fn expr2))))

;; TODO | and || operators are escaped -- can this even be used ? should we find an alternative ?
(defparameter rules-rr-binops
  (a:flatten (loop for item in '(:* :/ :% :+ :-
                                 :<< :>> :>>>
                                 :< :> :<= :>=
                                 :== :!= :=== :!==
                                 :instanceof in
                                 :& :^ :\|
                                 :&& :\|\|)
                   collect (cons item #'rr/binop))))

(defun rr/unary/pre (ast parse-fn)
  "rewrite unary prefix operations"
  (destructuring-bind (op expr1) ast
    `(://unary/pre ,(fmt-ident op) ,(funcall parse-fn expr1))))

(defparameter rules-rr-unary-pre
  (a:flatten (loop for item in '(:~ :delete :typeof :void)
                   collect (cons item #'rr/unary/pre))))

(defun rr/unary/pre/alias (op)
  (let ((op-ident (fmt-ident op)))
    (lambda (ast parse-fn)
      (destructuring-bind (alias_ expr) ast
        (declare (ignore alias_))
        `(://unary/pre ,op-ident ,(funcall parse-fn expr))))))

(defun rr/unary/post/alias (op)
  (let ((op-ident (fmt-ident op)))
    (lambda (ast parse-fn)
      (destructuring-bind (alias_ expr) ast
        (declare (ignore alias_))
        `(://unary/post ,op-ident ,(funcall parse-fn expr))))))

(defun rr/prop-access (ast parse-fn)
  (unless (symbolp (cadr ast))
    (error "invalid - property access (@ ...) form *must* start with a symbol"))
  (let ((xyz (reduce (lambda (acc e)
                       (if (keywordp e)
                           (cons e acc)
                           (list `(://prop[] ,(cons ://prop (reverse acc))
                                             ,(funcall parse-fn e))))) (cdr ast) :initial-value '())))
    (if (eq (car xyz) ://prop[])
        xyz
        `(://prop ,@(reverse xyz)))))

(defun rewrite-js-ast (ast)
  (rewrite-ast-multipass ast
                         '(:defun rr/defun
                           :if rr/if)
                         '(:lambda rr/lambda)
                         rules-rr-binops
                         (list :->num (rr/unary/pre/alias '+)
                               :negate (rr/unary/pre/alias '-)
                               :not (rr/unary/pre/alias '!)
                               :inc (rr/unary/post/alias '++)
                               :dec (rr/unary/post/alias '--))
                         rules-rr-unary-pre))
