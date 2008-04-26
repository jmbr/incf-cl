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

(defun ensure-and-copy-list (x)
  (if (listp x)
      (copy-list x)
      (list x)))

(defun nest-list (function initial-values &key test test-not n (m 1) max)
  "Returns a list of the results of applying FUNCTION successively to
INITIAL-VALUES and continuing until applying TEST (respectively
TEST-NOT) to the result is non NIL.

If M is specified, then NEST-LIST supplies the M most recent results
as arguments for TEST (respectivelly TEST-NOT) at each step.

If MAX is specified then FUNCTION is applied at most MAX times."
  (check-type function function)
  (when max
    (assert (not (minusp max))))
  (let* ((initial-values (ensure-and-copy-list initial-values))
         (test (if (or test test-not)
                   (canonicalize-test test test-not)
                   nil))
         (n (or n (length initial-values)))
         (max (or max -1)))
    (%nest-list function initial-values test n m max)))

(defun %nest-list (function initial-values test n m max)
  (assert (and (>= n 0) (>= m 0)))
  (do ((max max (1- max))
       (splice
        (last initial-values)
        (setf (cdr splice) (cons (apply function (last initial-values n))
                                 nil))))
      ((or (= max 0)
           (if test
               (not (apply test (last initial-values m)))
               nil))
       initial-values)))

;; (defun %nest-list (function initial-values test n m max)
;;   (if (or (= max 0)
;;           (not (apply test (last initial-values m))))
;;       initial-values
;;       (%nest-list function
;;                   (append initial-values
;;                           (list
;;                            (apply function (last initial-values n))))
;;                   test
;;                   n
;;                   m
;;                   (1- max))))
