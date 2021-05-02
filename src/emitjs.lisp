(in-package #:cl2js)

;; TODO: lambda
;; TODO: (return expr) // return expr;
;; TODO: (const ident expr) || const ident expr;
;; TODO: (let ident expr) || let ident expr;
;; TODO: (set ident expr) || let x = expr; (OR is it 'x = expr;' then?)
;; TODO array stx
;; TODO obj stx
;; TODO if-expr
;; TODO if-elif-else (cond)
;; TODO for (?)
;; TODO foreach (?)
;; TODO while
;; TODO labels, break
;; TODO switch case

;; PROPERTY and METHOD access
;; TODO (.- obj prop) || obj.prop
;; TODO: (. obj method ...args) || obj.method(...args)

;; (foo 1 2) -> foo(1, 2)
(defun emit/funcall (ast cp emit-ast)
  (declare (ignore emit-ast))
  (destructuring-bind (fname &rest args) ast
    (emit cp (format nil "~a(~{~a~^, ~})" fname args))))

;; (lambda (x) x)
(defun emit/lambda (ast cp emit-ast)
  ;; TODO: could emit single-line expr in cases where body is a list of 1
  (destructuring-bind (_ args &rest body) ast
    (declare (ignore _))
    (emit cp (format nil "(~{~a~^, ~}) => {" args))
    (emit-freshline cp)
    (incf (indent-level cp))
    (loop for elem in body
          do (funcall emit-ast elem))
    (decf (indent-level cp))
    (emit-freshline cp)
    (emit cp "}")
    (emit-freshline cp)))

;; return <expr>;
(defun emit/return (ast cp emit-ast)
  (emit cp "return ")
  (funcall emit-ast (cdr ast))
  (emit cp ";")
  (emit-freshline cp))

(defun emit-ast (ast cp emitters)
  "emit code of AST using given emitters"
  (labels ((emit-ast* (ast)
             (cond ((atom ast) (emit cp (format nil "~a" ast)))
                   ((symbolp (car ast))
                    (let ((emitter (getf emitters (car ast))))
                      (if emitter
                          (funcall emitter ast cp #'emit-ast*)
                          (emit/funcall ast cp #'emit-ast))))
                   (t (error "no emitter for value '~a'" ast)))))
    (emit-ast* ast)))

(defun emit-js (ast cp)
  (emit-ast ast cp '(defun emit/defun
                     lambda emit/lambda
                     return emit/return)))
