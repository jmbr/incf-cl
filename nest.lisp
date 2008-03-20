(in-package :com.superadditive.incf-cl)

;;; Copyright (c) 2008 Juan M. Bello Rivas <jmbr@superadditive.com>
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

;;; These functions are EXPERIMENTAL:

(defun nest (f arg &key test test-not m max)
  (assert (plusp m))
  (when max (assert (plusp max)))
  (let ((test (get-test-function test test-not))
        (args (list arg)))
    (dotimes (i (1- m))
      (push (funcall f (first args)) args))
    (do* ((count m (1+ count))
          (x (first args) (funcall f x))
          (args args (take m (cons x args))))
         ((or (if max (> count max) nil)
              (not (apply test (nreverse args))))
          x))))

(defun nest-list (function initial-value &key test test-not (m 1) max)
  "Returns a list of the results of applying F successively to ARG and
continuing until applying TEST (respectively TEST-NOT) to the result
no longer returns T.

If M is specified, the function supplies the M most recent results as
arguments for TEST (respectivelly TEST-NOT) at each step.

If MAX is specified then F is applied at most MAX times."
  (assert (plusp m))
  (when max (assert (plusp max)))
  (let ((test (get-test-function test test-not))
        (args (list initial-value))
        (results (cons initial-value nil)))
    (dotimes (i (1- m))
      (push (funcall function (first args)) args))
    (do* ((count m (1+ count))
          (x (first args) (funcall function x))
          (xs results (cdr (rplacd xs (cons x nil))))
          (args args (take m (cons x args))))
         ((or (if max (> count max) nil)
              (not (apply test (nreverse args))))
          results))))
