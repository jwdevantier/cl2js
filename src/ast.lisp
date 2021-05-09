(defpackage #:cl2js/ast
  (:use #:cl #:cl2js/utils)
  (:local-nicknames (:a :alexandria))
  (:export #:code-printer
           #:rewrite-ast
           #:rewrite-ast-multipass
           #:keywordize-ast
           #:ensure-return
           #:rr/let
           #:rr/if
           #:rr/cond
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
  (reduce #'rewrite-ast* rulesets :initial-value (keywordize-ast ast)))

(defun kw-of-string (s)
  "create/get keyword given string"
  (intern (string-upcase s) "KEYWORD"))

(defun keywordize-ast (ast)
  "convert all symbols to keywords in ast"
  (cond ((eq nil ast) ast)
        ((symbolp ast) (kw-of-string ast))
        ((atom ast) ast)
        (t (mapcar #'keywordize-ast ast))))

(defun ensure-return (stmts)
  "ensure last form in `stmts` is a :return-form"
  (unless (and (listp stmts)
               (not (eq stmts nil)))
    (error "ensure return expects a list of elements"))
  (let ((last-form (let ((last* (car (last stmts))))
                     (cond ((atom last*) `(:return ,last*))
                           ((listp last*)
                            (if (eq :return (car last*))
                                last*
                                `(:return ,last*)))))))
    (reduce (lambda (acc e) (cons e acc))
            (reverse (butlast stmts 1))
            :initial-value (cons last-form '()))))

(defun ->list (x)
  "coerce to list, wraps x in list if x is an atom (or nil)"
  (if (or (eq nil x)
          (not (listp x)))
      (list x) x))

(defun rr/let (ast parse-fn)
  "create IIF whose arguments match the bindings"
  (destructuring-bind (kw_ bindings &rest body) ast
    (declare (ignore kw_))
    `((:lambda ,(mapcar #'car bindings)
        ,@(ensure-return (mapcar parse-fn body)))
      ,@(mapcar #'cadr bindings))))

(defun rr/if (ast parse-fn)
  ; create IIFE and wrap both branches in (:progn) with a guaranteed
  ; (:return ...) form as the last form.
  (destructuring-bind (kw_ cond then &optional (else nil else?)) ast
    (declare (ignore kw_))
    `((:lambda ()
        (://if ,(funcall parse-fn cond)
               ,(cons :progn (ensure-return (->list (funcall parse-fn then))))
               ,(cons :progn (if else?
                                 (ensure-return (->list (funcall parse-fn else)))
                                 (list '(:return :nil)))))))))

(defun rr/cond (ast parse-fn)
  ;; (cond ...clause)
  ;; clause ::= (test-form form*)
  ;; add (t (:return :nil)) clause to the end to ensure IIFE gets a return value
  (let ((clauses (mapcar (lambda (clause)
                           `(,(funcall parse-fn (car clause))
                             ,@(ensure-return (funcall parse-fn (cdr clause)))))
                         (cdr ast))))
    ;; extra parens in unless form are necessary.
    ;; This way, a non-match (nil) is spliced away while a match
    ;; means the last clause retains a set of parens
    `((:lambda ()
        (://cond ,@clauses
                 ,@(unless (eq :t (caar (last clauses)))
                     '((:t (:return :nil)))))))))

;; should rewrite let and iter to wrap single-forms into a list
(defun rr/for (ast parse-fn)
  (destructuring-bind (kw_ opts &rest body) ast
    (declare (ignore kw_))
    (destructuring-bind (&key ((:let let-clause) :nil let?) (test :nil test?) (iter :nil iter?)) opts
      (let ((let-clause (if (binding? let-clause)
                            (list let-clause) let-clause)))
        ;; TODO: how to check for iter-component ?
        ;;    --- AND change (funcall parse-fn iter) -> (mapcar parse-fn iter-exprs)
        (when let?
          (loop for item in let-clause
                do (unless (binding? item)
                     (error (format nil "error in :let of for-form: '~a' is not a binding" item)))))
        `((:lambda ()
            (:for (:let ,(if let? (mapcadr parse-fn let-clause) :nil)
                   :test ,(if test? (funcall parse-fn test) :nil)
                   :iter ,(if iter? (funcall parse-fn iter) :nil))
                  ,@(mapcar parse-fn body))
            (:return :nil)))))))

(defun rr/defun (ast parse-fn)
  "desugar function declaration into let assignment of a fat-arrow function"
  ;; in (defun foo (...args) &body)
  ;; out (let foo (lambda (...args) &body))
  (destructuring-bind (_ fname args &rest body) ast
    (declare (ignore _))
    `(://defun ,fname (:lambda ,args ,@(mapcar parse-fn body)))))

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
  (let ((lst (reduce (lambda (acc e)
                       (if (keywordp e)
                           (cons e acc)
                           (list `(://prop[] ,(if (eq (length acc) 1)
                                                  (car acc)
                                                  (cons ://props (reverse acc)))
                                             ,(funcall parse-fn e))))) (cdr ast) :initial-value '())))
    (if (eq (length lst) 1)
        (car lst)
        `(://prop ,@(reverse lst)))))

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
