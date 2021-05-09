(defpackage #:cl2js/utils
  (:use cl)
  (:export #:fmt-ident
           #:dash-to-camel-case
           #:destructuring-plist
           #:cons-pair?
           #:binding?
           #:mapcadr))

(in-package #:cl2js/utils)

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

(defmacro destructuring-plist (keys expr &rest body)
  `(destructuring-bind (&key ,@keys &allow-other-keys) ,expr
     ,@body))

(defun cons-pair? (cp)
  (and (consp cp)
       (eq nil (cddr cp))))

(defun binding? (b)
  (and (consp b)
       (eq 2 (length b))
       (keywordp (car b))))

(defmacro mapcadr (function list &rest args)
  `(mapcar (lambda (e)
             (list (car e) (funcall ,function (cadr e)))) ,list ,@args))
