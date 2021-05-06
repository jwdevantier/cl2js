(defpackage #:cl2js/utils
  (:use cl)
  (:export #:fmt-ident
           #:dash-to-camel-case))

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
