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

;;; The current implementation is suboptimal due to its reliance on
;;; NEST-LIST.  This should change after a proper NEST is implemented.

(defun fixed-point (function initial-value &key (test #'eql) max-steps)
  "Returns the fixed point of FUNCTION starting with INITIAL-VALUE.
The keyword argument TEST determines when a fixed point has been
reached.  Use MAX-STEPS to stop after a certain (positive) number of
iterations.

For example, the square root of 2 using Newton's method can be
computed as:

INCF-CL> (fixed-point (lambda (x)
                        (float (- x (/ (- (expt x 2) 2) (* 2 x)))))
                      1)
1.4142135

INCF-CL> (sqrt 2)
1.4142135"
  (first (last (nest-list function
                          initial-value :n 1
                          :test-not test :m 2
                          :max (if max-steps
                                   (+ 2 max-steps)
                                   nil)))))
