(in-package :com.superadditive.incf-cl)

;;; Copyright (c) 2007 Juan M. Bello Rivas <jmbr@superadditive.com>
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defmacro signals-p (condition &body body)
  "Returns T if evaluating BODY results in CONDITION being signalled,
NIL otherwise."
  `(let ((sym (gensym "SIGNALS-P")))
     (eq sym (handler-case (progn ,@body)
               (,condition () sym)))))

(define-condition doctest-failure ()
  ((sexpr :initarg :sexpr :reader sexpr)
   (actual-values :initarg :actual-values :reader actual-values)
   (expected-values :initarg :expected-values :reader expected-values))
  (:report (lambda (condition stream)
             (format stream "~s => ~{~a~^ ~} /= ~{~a~^ ~}"
                     (sexpr condition)
                     (actual-values condition)
                     (expected-values condition)))))

(defun symbol-function-or-nil (symbol)
  ;; Taken verbatim from the Hyperspec.
  (if (and (fboundp symbol)
           (not (macro-function symbol))
           (not (special-operator-p symbol)))
      (symbol-function symbol)
      nil))

(defun test-docstring (documentation)
  "Returns T if the first doctest found in DOCUMENTATION passes,
signals DOCTEST-FAILURE otherwise."
  (with-input-from-string (input-stream documentation)
    (labels ((aux (_)
               (declare (ignore _))
               (read input-stream nil input-stream)))
      (let* ((sexpr (read input-stream))
             (actual-values (multiple-value-list (eval sexpr)))
             (expected-values (mapcar #'aux actual-values))
             (eof-pos (position-if (slice #'eq input-stream)
                                   expected-values)))
        (if (every #'equalp actual-values expected-values)
            t
            (signal 'doctest-failure
                    :sexpr sexpr
                    :actual-values actual-values
                    :expected-values (if eof-pos
                                         (take eof-pos expected-values)
                                         expected-values)))))))

(defun test-function (package-name function stream)
  "Returns T if every test written in FUNCTION's docstring passes, NIL
otherwise."
  (let* ((passed-p t)
         (documentation (documentation function 'function))
         (re (concatenate 'string "[ \t]*" package-name "> "))
         (matches (cl-ppcre:all-matches re documentation)))
    (loop for start in (rest matches) by #'cddr do
         (handler-case
             (when (test-docstring (subseq documentation start))
               (princ #\. stream))
           (end-of-file (_)
             (declare (ignore _))
             (format stream "~%MALFORMED TEST: ~a~%" function))
           (doctest-failure (condition)
             (format stream "~%FAILED TEST: ~a~%~a~%" function condition)
             (setf passed-p nil)))
         finally (return passed-p))))

(defun doctest (package
                &key (function nil function-p) (stream *standard-output*))
  "Tests docstrings present in functions defined in PACKAGE and
outputs its results to STREAM.  It returns T if the tests succeed, NIL
on error.  If FUNCTION is specified, DOCTEST will check that function
only.  By default, DOCTEST will test each function exported by
PACKAGE."
  (let ((passed-p t)
        (package-name (etypecase package
                        (string (string-upcase package))
                        (symbol (symbol-name package))))
        (*package* (find-package package)))
    (if function-p
        (test-function package-name function stream)
        (do-external-symbols (symbol package passed-p)
          (let ((function (symbol-function-or-nil symbol)))
            (when function
              (unless (test-function package-name function stream)
                (setf passed-p nil))))))))
