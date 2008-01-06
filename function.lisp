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

(defmacro slice (function &rest args)
  ;; Inspired by iterate's sharpl reader macro and
  ;; http://srfi.schemers.org/srfi-26/srfi-26.html
  (let ((lambda-list nil)
        (parameters nil)
        (remaining-args (gensym "SLICE")))
    (dolist (x args)
      (let ((s (gensym "SLICE")))
        (when (eq x '_)
          (push s lambda-list))
        (push s parameters)))
    `(lambda (,@(nreverse lambda-list) &rest ,remaining-args)
       (apply ,function ,@(nreverse parameters) ,remaining-args))))

(defun $ (&rest functions)
  "Returns the composition of FUNCTIONS.  Note that FUNCTIONS must 
  be composable in the order specified.

  For example,

  INCF-CL> (funcall ($ (lambda (x) (* x x))
                       (lambda (x) (+ x 2)))
                    2)
  16

  INCF-CL> (funcall ($ #'values #'floor #'sqrt (slice #'expt _ 2)) -2)
  2"
  (assert (every #'functionp functions))
  (flet ((compose (f g)
           (lambda (&rest args)
             (funcall f (apply g args)))))
    (reduce #'compose functions)))
