(in-package #:cl2js)

(defun rewrite-ast (ast ruleset)
  "parse AST, applying rewrite ruleset"
  (labels ((rewrite-ast* (ast)
           (cond ((atom ast) ast)
                 ((symbolp (car ast))
                  (let ((rewrite-rule (getf ruleset (car ast))))
                    (if rewrite-rule
                        (funcall rewrite-rule ast #'rewrite-ast*)
                        (mapcar #'rewrite-ast* ast))))
                 (t (mapcar #'rewrite-ast* ast)))))
    (rewrite-ast* ast)))

(defun rewrite-ast-multipass (ast &rest rulesets)
  "parse AST, applying each ruleset in turn."
  (reduce #'rewrite-ast rulesets :initial-value ast))

(defun rr/defun (ast parse-fn)
  "desugar function declaration into let assignment of a fat-arrow function"
  ;; in (defun foo (...args) &body)
  ;; out (let foo (lambda (...args) &body))
  (declare (ignore parse-fn))
  (destructuring-bind (_ fname args &rest body) ast
    (declare (ignore _))
    `(let ,fname (lambda ,args ,@body))))

(defun rr/lambda (ast parse-fn)
  "rewrite lambda to return result of last stmt"
  (declare (ignore parse-fn))
  (destructuring-bind (_ args &rest body) ast
    (declare (ignore _))
    (let ((stmts (butlast body 1))
          (last-stmt (car (last body))))
      (if (and (listp last-stmt) (eq (car last-stmt) 'return))
          ast
          `(lambda ,args ,@stmts (return ,last-stmt))))))
