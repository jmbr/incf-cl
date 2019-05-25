(cl:in-package #:incf-cl)

;;; Copyright (c) 2008-2019 Juan M. Bello Rivas <jmbr@superadditive.com>
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
         (l (length initial-values))
         (n (or n l))
         (test (if (or test test-not)
                   (canonicalize-test test test-not)
                   nil))
         (max (or max -1)))
    (%nest-list function
                (if (> m l)
                    (%nest-list function initial-values nil n 0 (- m l))
                    initial-values)
                test
                n
                m
                max)))

(defun %nest-list (function initial-values test n m max)
  (assert (and (plusp n) (>= m 0)))
  (do ((max max (1- max))
       (splice
        (last initial-values)
        (setf (rest splice) (cons (apply function (last initial-values n))
                                  nil))))
      ((or (= max 0)
           (if test
               (not (apply test (last initial-values m)))
               nil))
       initial-values)))

(defun nest (function initial-values &key test test-not n (m 1) max)
  ;; XXX - This is good enough for the moment.
  (first (last (nest-list function
                          initial-values
                          :test test :test-not test-not
                          :n n :m m :max max))))
