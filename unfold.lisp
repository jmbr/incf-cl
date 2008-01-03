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


(defun unfold (predicate transformer incrementor initial-value
               &optional (tail-gen (constantly nil)))
  "Returns a list built following the pattern:

  transformer(initial-value), transformer(incrementor(initial-value)), ...

  Examples:

  1. List of squares: 1^2, ..., 10^2

  INCF-CL> (unfold (slice #'> _ 10) (slice #'expt _ 2) #'1+ 1)
  (1 4 9 16 25 36 49 64 81 100)

  2. Append (3 4 5) onto (1 2)

  INCF-CL> (unfold #'null #'first #'rest (list 1 2) (lambda (x)
                                                    (declare (ignore x))
                                                    (list 3 4 5)))
  (1 2 3 4 5)

  See also: 
    http://srfi.schemers.org/srfi-1/srfi-1.html#unfold"
  (let ((result (cons nil nil)))
    (do ((item initial-value (funcall incrementor item))
         (splice result (rest (rplacd splice
                                      (cons (funcall transformer item) nil)))))
        ((funcall predicate item) (nconc (rest result)
                                         (funcall tail-gen item))))))


(defun unfold-right (predicate transformer incrementor initial-value
                     &optional (tail nil))
  "Returns a list built following the pattern:

  ... transformer(incrementor(initial-value)), transformer(initial-value)

  Examples:

  1. List of squares: 1^2, ..., 10^2

  INCF-CL> (unfold-right #'zerop (lambda (x) (* x x)) #'1- 10)
  (1 4 9 16 25 36 49 64 81 100)

  2. Reverse a proper list:

  INCF-CL> (unfold-right #'null #'first #'rest (list 1 2 3 4 5))
  (5 4 3 2 1)

  See also: 
    http://srfi.schemers.org/srfi-1/srfi-1.html#unfold-right"
  (do ((item initial-value (funcall incrementor item))
         (result tail (cons (funcall transformer item) result)))
        ((funcall predicate item) result)))
