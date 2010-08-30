(cl:in-package #:incf-cl)

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

(defgeneric unscan (function list &key initial-value)
  (:documentation "Semi-inverse of SCAN*.

  INCF-CL> (equal (unscan (flip #'-) (scan* #'+ '(1 2 3)) :initial-value 0)
                  '(1 2 3))
  T")
  (:argument-precedence-order list function))

(defmethod unscan ((function function) (list null) &key initial-value)
  (declare (ignore function initial-value))
  nil)

(defmethod unscan ((function function) (list list) &key initial-value)
  (destructuring-bind (c . cs) list
    (cons (funcall function initial-value c)
          (unscan function cs :initial-value c))))
