(cl:in-package #:incf-cl)

;;; Copyright (c) 2007-2019 Juan M. Bello Rivas <jmbr@superadditive.com>
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

;;; Most of the docstrings are taken from A tour of the Haskell
;;; Prelude by Bernie Pope.

(deftype function-or-null () `(or function null))

(declaim (inline apply-key))
(defun apply-key (key element)
  ;; This is useful for saving a function call
  (if key
      (funcall key element)
      element))

(declaim (inline first-with))
(defun first-with (key list)
  (apply-key key (first list)))

(defgeneric break* (predicate list &key key)
  (:documentation "Given a PREDICATE and a LIST, breaks LIST into two
lists (returned as VALUES) at the point where PREDICATE is first
satisfied.  If PREDICATE is never satisfied then the first returned
value is the entire LIST and the second element is NIL.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list predicate))

(defmethod break* ((predicate function) (list list) &key key)
  (span (complement predicate) list :key key))

(defgeneric cycle (list)
  (:documentation "Returns a circular list containing the elements in
LIST (which should be a proper list)."))

(defmethod cycle ((list null))
  nil)

(defmethod cycle ((list list))
  (ncycle (copy-list list)))

(defgeneric ncycle (list)
  (:documentation "Destructive version of CYCLE.  Again, keep in mind
  that LIST must be a proper list."))

(defmethod ncycle ((list null))
  ;; Defined for consistency with CYCLE (LAST expects a CONS).
  nil)

(defmethod ncycle ((list list))
  (setf (rest (last list)) list))

(defgeneric drop (n list)
  (:documentation "Applied to N (a non-negative integer) and LIST,
returns the list with the specified number of elements removed from
the front of LIST.  If LIST has less than N elements then it returns
NIL.")
  (:argument-precedence-order list n))

(defmethod drop ((n integer) (list list))
  (unless (minusp n)
    (nthcdr n list)))

(defgeneric drop-while (predicate list &key key)
  (:documentation "Applied to PREDICATE and LIST, removes elements
from the front of LIST while PREDICATE is satisfied.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list predicate))

(defmethod drop-while ((predicate function) (list list) &key key)
  (check-type key function-or-null)
  (do ((list list (rest list)))
        ((or (endp list)
             (not (funcall predicate (first-with key list)))) list)))

(defgeneric partition (predicate list &key key)
  (:documentation "Applied to PREDICATE and LIST, returns two values: a list
containing all the elements from LIST that satisfy PREDICATE, and its
complementary list.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list predicate))

(defmethod partition ((predicate function) (list list) &key key)
  (check-type key function-or-null)
  (let* ((result1 (cons nil nil))
         (result2 (cons nil nil))
         (splice1 result1)
         (splice2 result2))
    (dolist (x list (values (rest result1) (rest result2)))
      (let ((c (cons x nil))
            (elem (apply-key key x)))
        (if (funcall predicate elem)
            (setf splice1 (setf (rest splice1) c))
            (setf splice2 (setf (rest splice2) c)))))))

(defun flip (f)
  "Applied to a binary function F, returns the same function with the
order of the arguments reversed."
  (lambda (x y)
    (funcall f y x)))

(defgeneric insert (x list &key key test test-not)
  (:documentation "Inserts X before the first element in LIST which is
greater than X.  The order relation can be specified by either one of
the keyword arguments TEST and TEST-NOT.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list x))

(defmethod insert (x (list list) &key key (test #'<) test-not)
  (check-type key function-or-null)
  (let ((test (canonicalize-test test test-not)))
    (multiple-value-bind (lt ge) (span (lambda (z)
                                         (funcall test z (apply-key key x)))
                                       list :key key)
      (nconc lt (cons x ge)))))

(defun replicate (n x)
  "Returns a list contaning N times the value X"
  (when (plusp n)
    (loop repeat n collect x)))

(defgeneric span (predicate list &key key)
  (:documentation "Splits LIST into two lists (returned as VALUES)
such that elements in the first list are taken from the head of LIST
while PREDICATE is satisfied, and elements in the second list are the
remaining elements from LIST once PREDICATE is not satisfied.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list predicate))

(defmethod span ((predicate function) (list list) &key key)
  (check-type key function-or-null)
  (let ((result (cons nil nil)))
      (do ((list list (rest list))
           (splice result (setf (rest splice) (cons (first list) nil))))
          ((or (endp list)
               (not (funcall predicate (first-with key list))))
           (values (rest result) list)))))

(defgeneric split-at (n list)
  (:documentation "Given a non-negative integer N and LIST, splits
LIST into two lists (returned as VALUES) at the position corresponding
to the given integer.  If N is greater than the length of LIST, it
returns the entire list first and the empty list second in VALUES.")
  (:argument-precedence-order list n))

(defmethod split-at ((n integer) (list list))
  (unless (minusp n)
    (let ((result (cons nil nil)))
      (do ((list list (rest list))
           (n n (1- n))
           (splice result (setf (rest splice) (cons (first list) nil))))
          ((or (zerop n)
               (endp list))
           (values (rest result) list))))))

(defgeneric take (n list)
  (:documentation "Applied to the integer N and LIST, returns the specified number of
elements from the front of LIST.  If LIST has less than N elements,
TAKE returns the entire LIST.")
  (:argument-precedence-order list n))

(defmethod take ((n integer) (list list))
  (values (funcall #'split-at n list)))

(defgeneric take-while (predicate list &key key)
  (:documentation "Applied to PREDICATE and LIST, returns a list
containing elements from the front of LIST while PREDICATE is
satisfied.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.")
  (:argument-precedence-order list predicate))

(defmethod take-while ((predicate function) (list list) &key key)
  (values (span predicate list :key key)))

(defun unzip (alist)
  "Applied to the association list ALIST, returns two lists (as
VALUES) containing the keys and values of each element in ALIST
respectively.  This function is the inverse of PAIRLIS."
  (when (listp alist)
    (do ((alist alist (rest alist))
         (xs nil (cons (caar alist) xs))
         (ys nil (cons (cdar alist) ys)))
        ((endp alist) (values xs ys)))))

(defgeneric scan* (function list &key key from-end initial-value)
  (:documentation "SCAN* is similar to REDUCE, but returns a list of
  successive reduced values:

  (scan* f (list x1 x2 ...) :initial-value z)
  ==> (z (funcall f z x1) (funcall f (funcall f z x1) x2) ...)

  (scan* f (list x1 x2 ...))
  ==> (x1 (funcall f x1 x2) (funcall f (funcall f x1 x2) x2) ...)

  (scan* f (list x1 ... x_n-1 x_n) :initial-value z :from-end t)
  ==> (... (funcall f x_n-1 (funcall f x_n z)) (funcall f x_n z) z)

  (scan* f (list x1 ... x_n-1 x_n) :from-end t)
  ==> (... (funcall f x_n-1 (funcall f x_n-1 x_n)) (funcall f x_n-1 x_n) x_n)

  Examples:

  INCF-CL> (scan* #'/ (list 4 2 4) :initial-value 64)
  (64 16 8 2)

  INCF-CL> (scan* #'max (range 1 7) :initial-value 5)
  (5 5 5 5 5 5 6 7)

  INCF-CL> (scan* (lambda (x y) (+ (* 2 x) y)) (list 1 2 3) :initial-value 4)
  (4 9 20 43)

  INCF-CL> (scan* #'+ (list 1 2 3 4))
  (1 3 6 10)

  INCF-CL> (scan* #'+ (list 1 2 3 4) :initial-value 5 :from-end t)
  (15 14 12 9 5)

  INCF-CL> (scan* #'+ (list 1 2 3 4) :from-end t)
  (10 9 7 4)")
  (:argument-precedence-order list function))

(declaim (inline scan-left*))
(defun scan-left* (function list key initial-value ivp)
  (let ((result (cons (if ivp
                          initial-value
                          (first-with key list))
                      nil)))
    (do ((list (if ivp list (rest list)) (rest list))
         (splice
          result
          (setf (rest splice) (cons (funcall function
                                             (first splice)
                                             (first-with key list))
                                    nil))))
        ((endp list) result))))

(declaim (inline scan-right*))
(defun scan-right* (function list key initial-value ivp)
  (let ((list (reverse list)))
    (do ((list (if ivp list (rest list)) (rest list))
         (result
          (cons (if ivp initial-value (first-with key list))
                nil)
          (cons (funcall function (first-with key list) (first result))
                result)))
        ((endp list) result))))

(defmethod scan* ((function function) (list list)
                  &key key from-end (initial-value nil ivp))
  (check-type key function-or-null)
  (if ivp                               ; Hrm...
      (check-type list list)
      (check-type list cons))
  (if from-end
      (scan-right* function list key initial-value ivp)
      (scan-left* function list key initial-value ivp)))

(defgeneric intersperse (element list)
  (:documentation "Returns a list where ELEMENT is interspersed
between the elements of SEQUENCE.

  For example,

  INCF-CL> (intersperse 'x (replicate 3 'z))
  (Z X Z X Z)")
  (:argument-precedence-order list element))

(defmethod intersperse (element (list null))
  (declare (ignore element))
  nil)

(defmethod intersperse (element (list cons))
  (let ((result (cons nil nil)))
    (do ((list list (rest list))
         (splice result
                 (rest (setf (rest splice) (cons (first list)
                                                 (cons element nil))))))
        ((endp (rest list))
         (rplacd splice (cons (first list) nil))
         (rest result)))))

(defgeneric nintersperse (element list)
  (:documentation "Destructive version of INTERSPERSE.")
  (:argument-precedence-order list element))

(defmethod nintersperse (element (list list))
  (do ((xs list (let ((c (cons element (rest xs))))
                  (rest (setf (rest xs) c)))))
      ((endp (rest xs)) list)))

(defgeneric group (list &key key test test-not)
  (:documentation "Returns a list of lists where every item in each
sublist satisfies TEST and the concatenation of the result is equal to
LIST.

  KEY is a designator for a function of one argument, or NIL.  If KEY
is supplied, it is applied once to each element of LIST before it is
passed to PREDICATE.  If it is not supplied or is NIL, the element of
LIST itself is used.

  For example,

  INCF-CL> (mapcar (lambda (x) (concatenate 'string x))
                   (group (coerce \"Mississippi\" 'list)))
  (\"M\" \"i\" \"ss\" \"i\" \"ss\" \"i\" \"pp\" \"i\")"))

(defmethod group ((list list) &key key (test #'eql) test-not)
  (check-type key function-or-null)
  (let* ((test (canonicalize-test test test-not))
         (result (cons nil nil))
         (splice result))
    (do ()
        ((endp list) (rest result))
      (destructuring-bind (x . xs) list
        (multiple-value-bind (ys zs) (span (lambda (z)
                                             (funcall test z (apply-key key x)))
                                           xs :key key)
          (setf splice (setf (rest splice) (list (cons x ys))))
          (setf list zs))))))
