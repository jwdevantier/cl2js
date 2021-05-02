(in-package #:cl2js)

(defvar *codegen-output* *standard-output*)

(defclass code-printer ()
  ((code :accessor out
         :initarg out
         :initform *codegen-output*)
   (indent-level :accessor indent-level
                 :initarg :indent-level
                 :initform 0)
   (indent-by :accessor indent-by
              :initarg :indent-by
              :initform "   ")
   (beginning-of-line-p :accessor beginning-of-line-p
                        :initform t)))

(defun indent-if-necessary (cp)
  (when (beginning-of-line-p cp)
    (loop repeat (indent-level cp)
          do (write-sequence (indent-by cp) (out cp)))
    (setf (beginning-of-line-p cp) nil)))

(defun emit-newline (cp)
  (write-char #\Newline (out cp))
  (setf (beginning-of-line-p cp) t))

(defun emit-freshline (cp)
  (unless (beginning-of-line-p cp)
    (emit-newline cp)))

(defun emit/no-newlines (cp string &key (start 0) end)
  (indent-if-necessary cp)
  (write-sequence string (out cp) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p cp) nil)))

(defun emit (cp string)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\Newline string :start start)
        do (emit/no-newlines cp string :start start :end pos)
        when pos do (emit-newline cp)
          while pos))

(defmacro with-codegen-output ((stream) &body body)
  "redirect code output"
  `(let* ((*codegen-output* ,stream))
     ,@body))

;; -- example - emitting code to file
;; (with-open-file (out "/tmp/file.js" :direction :output)
;;   (with-codegen-output (out)
;;     (let ((cp (make-instance 'code-printer)))
;;       (emit cp "const add3(x) =>")
;;       (emit-freshline cp)
;;       (incf (indent-level cp))
;;       (emit cp "x + 3"))))
