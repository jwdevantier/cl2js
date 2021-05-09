(in-package #:cl2js)



(defun split-by-hyphen (str)
  (loop for i = 0 then (1+ j)
        as j = (position #\- str :start i)
        collect (subseq str i j)
        while j))

(defun dash-to-camel-case (ident)
  (destructuring-bind (acc &rest rest) (split-by-hyphen ident)
    (dolist (item rest)
      (setq acc (concatenate 'string acc (string-capitalize item))))
    acc))

(defun fmt-ident (ident)
  (if (stringp ident)
      ident
      (dash-to-camel-case (string-downcase (string ident)))))

;; (foo 1 2) -> foo(1, 2)
(defun emit/funcall (ast cp emit-ast)
  (destructuring-bind (fname &rest args) ast
    (emit cp (fmt-ident fname))
    (emit cp "(")
    (labels ((nxt (x xs)
               (funcall emit-ast x)
               (when xs
                 (progn (emit cp ", ")
                        (nxt (car xs) (cdr xs))))))
      (nxt (car args) (cdr args)))
    (emit cp ")")))

;; (lambda (x) x)
(defun emit/lambda (ast cp emit-ast)
  ;; TODO: could emit single-line expr in cases where body is a list of 1
  (destructuring-bind (_ args &rest body) ast
    (declare (ignore _))
    (emit cp (format nil "(~{~a~^, ~}) => {" (mapcar #'fmt-ident args)))
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
  (funcall emit-ast (cadr ast))
  (emit cp ";")
  (emit-freshline cp))

;; (let x <expr>)
(defun emit/let (ast cp emit-ast)
  (destructuring-bind (kw_ ident expr) ast
    (declare (ignore kw_))
    (emit cp (format nil "let ~a = " (fmt-ident ident)))
    (funcall emit-ast expr)
    (emit cp ";")))

(defun emit/const (ast cp emit-ast)
  (destructuring-bind (kw_ ident expr) ast
    (declare (ignore kw_))
    (emit cp (format nil "const ~a = " (fmt-ident ident)))
    (funcall emit-ast expr)
    (emit cp ";")))

(defun emit/set (ast cp emit-ast)
  (destructuring-bind (kw_ ident expr) ast
    (declare (ignore kw_))
    (emit cp (format nil "~a = " (fmt-ident ident)))
    (funcall emit-ast expr)
    (emit cp ";")
    (emit-freshline cp)))

;; (//binop <op> <expr1> <expr2>) -> <expr1> <op> <expr2>
(defun emit/binop (ast cp emit-ast)
  "emit binary operator expression from //binary forms"
  (destructuring-bind (kw_ op expr1 expr2) ast
    (declare (ignore kw_))
    (funcall emit-ast expr1)
    (emit cp (format nil " ~a " op))
    (funcall emit-ast expr2)))

(defun emit/unary/pre (ast cp emit-ast)
  "emit unary prefix operator"
  (destructuring-bind (kw_ op expr) ast
    (declare (ignore kw_))
    (emit cp (format nil "~a " op))
    (funcall emit-ast expr)))

(defun emit/unary/post (ast cp emit-ast)
  "emit unary postfix operator"
  (destructuring-bind (kw_ op expr) ast
    (declare (ignore kw_))
    (funcall emit-ast expr)
    (emit cp (format nil " ~a" op))))

(defun emit/prop. (ast cp emit-ast)
  (destructuring-bind (kw_ first &rest rest) ast
    (declare (ignore kw_))
    (funcall emit-ast first)
    (loop for prop in rest
          do (progn (emit cp ".")
                    (funcall emit-ast prop)))))

(defun emit/prop-get (ast cp emit-ast)
  (destructuring-bind (kw_ obj-expr get-expr) ast
    (declare (ignore kw_))
    (funcall emit-ast obj-expr)
    (emit cp "[")
    (funcall emit-ast get-expr)
    (emit cp "]")))

;; TODO: assume AST transform already handled progn/expr and return.
;; TODO: desugar (if expr then-clause) to (if expr then-clause else nil)
;; emit IIF
;; (() => { if (<cond>) <then>; else <else>;})()
(defun emit/if-else (ast cp emit-ast)
  (destructuring-bind (if_ if-cond then-branch else-branch) ast
    (declare (ignore if_))
    (emit cp "(() => {")
    (emit-freshline cp)
    (incf (indent-level cp))
    (emit cp "if (")
    (funcall emit-ast if-cond)
    (emit cp ")")
    (emit-freshline cp)
    (incf (indent-level cp))
    (funcall emit-ast then-branch)
    (decf (indent-level cp))
    (emit-freshline cp)
    (emit cp "else")
    (emit-freshline cp)
    (incf (indent-level cp))
    (funcall emit-ast else-branch)
    (decf (indent-level cp))
    (emit-freshline cp)
    (emit cp "})()")
    (decf (indent-level cp))))

(defun emit-ast (ast cp emitters)
  "emit code of AST using given emitters"
  (labels ((emit-ast* (ast)
             (cond ((stringp ast) (emit cp (format nil "\"~a\"" ast)))
                   ((atom ast) (emit cp (format nil "~a" (if (symbolp ast) (fmt-ident ast) ast))))
                   ((symbolp (car ast))
                    (let ((emitter (getf emitters (car ast))))
                      (if emitter
                          (funcall emitter ast cp #'emit-ast*)
                          (emit/funcall ast cp #'emit-ast*))))
                   (t (error "no emitter for value '~a'" ast)))))
    (emit-ast* ast)))

(defun emit-js (ast cp)
  (emit-ast ast cp '(:lambda emit/lambda
                     :return emit/return
                     :let emit/let
                     :const emit/const
                     :set emit/set
                     ://binary emit/binop
                     ://unary/pre emit/unary/pre
                     ://unary/post emit/unary/post
                     ://prop emit/prop.
                     ://prop[] emit/prop-get
                     :if emit/if-else)))

;; *CONSIDER* not resolving ident iff already a str (escape-hatch)
;; refactor - move ident transformation out into separate fn
;; -- apply ident on let/const/funcall/setf(IMPLEMENT) and for args in funcall & lambda
