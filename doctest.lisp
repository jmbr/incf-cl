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



(defun symbol-function-or-nil (symbol)
  ;; Taken verbatim from the Hyperspec.
  (if (and (fboundp symbol)
           (not (macro-function symbol))
           (not (special-operator-p symbol)))
      (symbol-function symbol)
      nil))

(defun check (input-stream output-stream)
  (let ((sexpr (read input-stream nil input-stream)))
    (if (eq sexpr input-stream)
        nil
        (progn
          (let* ((actual-results (multiple-value-list (eval sexpr)))
                 (expected-results (mapcar #'(lambda (_)
                                               (declare (ignore _))
                                               (read input-stream nil input-stream))
                                           actual-results)))
            ;; The EOF check is a bit ugly but it's simple and works.
            (if (and (notany #'(lambda (x) (eq x input-stream)) expected-results)
                     (every #'equalp actual-results expected-results))
                (princ #\. output-stream)
                (progn
                  (format output-stream
                          "FAILED: ~s => ~{~a~^ ~}~%(expected ~{~a~^ ~})~%"
                          sexpr actual-results expected-results)
                  nil)))))))

(defun test-docstring (package-name function documentation stream)
  (let* ((passed t)
         (re (concatenate 'string "[ \t]*" package-name "> "))
         (matches (cl-ppcre:all-matches re documentation)))
    (when matches
      (format stream "~a~%" function)
      (loop for start in (rest matches) by #'cddr do
           (with-input-from-string (s documentation :start start)
             (unless (check s stream)
               (setf passed nil)))
           finally (terpri)))
    passed))

(defun doctest (package-name &optional (stream *standard-output*))
  "Checks the docstrings of each exported function in package
PACKAGE-NAME and outputs the results to STREAM."
  (let ((*package* (find-package package-name))
        (result t))
    (do-external-symbols (symbol package-name result)
      (let* ((function (symbol-function-or-nil symbol))
             (documentation (documentation function 'function)))
        (when (and function documentation)
          (unless (test-docstring package-name function documentation stream)
            (setf result nil)))))))
