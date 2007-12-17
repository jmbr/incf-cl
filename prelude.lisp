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

;;; Most of the docstrings are taken from A tour of the Haskell
;;; Prelude by Bernie Pope.

(defun break* (predicate list)
  "Given a PREDICATE and a LIST, breaks LIST into two lists (returned
as VALUES) at the point where PREDICATE is first satisfied.  If
PREDICATE is never satisfied then the first returned value is the
entire LIST and the second element is NIL."
  (span (complement predicate) list))

(defun cycle (list)
  "Returns a circular list containing the elements in LIST (which
should be a proper list)."
  (when (consp list)
    (ncycle (copy-list list))))

(defun ncycle (list)
  "Destructive version of CYCLE."
  (when (consp list)
    (rest (rplacd (last list) list))))

(defun drop (n list)
  "Applied to N (a non-negative integer) and LIST, returns the list
with the specified number of elements removed from the front of LIST.
If LIST has less than N elements then it returns NIL."
  (unless (minusp n)
    (nthcdr n list)))

(defun drop-while (predicate list)
  "Applied to PREDICATE and LIST, removes elements from the front of
LIST while PREDICATE is satisfied."
  (when (and (functionp predicate) (listp list))
    (do ((list list (rest list)))
        ((or (null list)
             (not (funcall predicate (first list)))) list))))

(defun partition (predicate list)
  "Applied to PREDICATE and LIST, returns two values: a list
containing all the elements from LIST that satisfy PREDICATE, and its
complementary list."
  (when (and (functionp predicate) (listp list))
    (let* ((result1 (cons nil nil))
           (result2 (cons nil nil))
           (splice1 result1)
           (splice2 result2))
      (dolist (x list (values (rest result1) (rest result2)))
        (let ((c (cons x nil)))
          (if (funcall predicate x)
              (setf splice1 (rest (rplacd splice1 c)))
              (setf splice2 (rest (rplacd splice2 c)))))))))

(defun flip (f)
  "Applied to a binary function F, returns the same function with the
order of the arguments reversed."
  (lambda (x y)
    (funcall f y x)))

(defun insert (x list &key (test #'<))
  "Inserts X before the first element in LIST which is greater than X.
The order relation can be specified by the keyword TEST"
  (when (and (listp list) (functionp test))
    (multiple-value-bind (lt ge) (span (curry (flip test) x) list)
      (nconc lt (cons x ge)))))

(defun replicate (n x)
  "Returns a list contaning N times the value X"
  (when (plusp n)
    (loop repeat n collect x)))

(defun span (predicate list)
  "Splits LIST into two lists (returned as VALUES) such that elements
in the first list are taken from the head of LIST while PREDICATE is
satisfied, and elements in the second list are the remaining elements
from LIST once PREDICATE is not satisfied."
  (when (and (functionp predicate) (listp list))
    (let ((result (cons nil nil)))
      (do ((list list (rest list))
           (splice result (rest (rplacd splice (cons (first list) nil)))))
          ((or (null list)
               (not (funcall predicate (first list))))
           (values (rest result) list))))))

(defun split-at (n list)
  "Given an integer N (positive or zero) and LIST, splits LIST into
two lists (returned as VALUES) at the position corresponding to the
given integer.  If N is greater than the length of LIST, it returns
the entire list first and the empty list second in VALUES."
  (when (and (>= n 0) (listp list))
    (let ((result (cons nil nil)))
      (do ((list list (rest list))
           (n n (1- n))
           (splice result (rest (rplacd splice (cons (first list) nil)))))
          ((or (zerop n)
               (null list))
           (values (rest result) list))))))

(defun take (n list)
  "Applied to the integer N and LIST, returns the specified number of
elements from the front of LIST.  If LIST has less than N elements,
TAKE returns the entire LIST."
  (values (funcall #'split-at n list)))

(defun take-while (predicate list)
  "Applied to PREDICATE and LIST, returns a list containing elements
from the front of LIST while PREDICATE is satisfied."
  (values (span predicate list)))

(defun unzip (alist)
  "Applied to the association list ALIST, returns two lists (as
VALUES) containing the keys and values of each element in ALIST
respectively.  This function is the inverse of PAIRLIS."
  (when (listp alist)
    (do ((alist alist (rest alist))
         (xs nil (cons (caar alist) xs))
         (ys nil (cons (cdar alist) ys)))
        ((null alist) (values xs ys)))))

(declaim (inline first-with))
(defun first-with (key list)
  (if key
      (funcall key (first list))
      (first list)))

(declaim (inline scan-left*))
(defun scan-left* (function list key initial-value ivp)
  (let ((result (cons (if ivp
                          initial-value
                          (first-with key list))
                      nil)))
    (do ((list (if ivp list (rest list)) (rest list))
         (splice
          result
          (rest (rplacd splice (cons (funcall function
                                              (first splice)
                                              (first-with key list))
                                     nil)))))
        ((null list) result))))

(declaim (inline scan-right*))
(defun scan-right* (function list key initial-value ivp)
  (let ((list (reverse list)))
    (do ((list (if ivp list (rest list)) (rest list))
         (result
          (cons (if ivp initial-value (first-with key list))
                nil)
          (cons (funcall function (first-with key list) (first result))
                result)))
        ((null list) result))))

(defun scan* (function list
              &key key from-end (initial-value nil ivp))
  "SCAN* is similar to REDUCE, but returns a list of successive
  reduced values:

  (scan* f (list x1 x2 ...) :initial-value z) 
  ==> (z (funcall f z x1) (funcall f (funcall f z x1) x2) ...)

  (scan* f (list x1 x2 ...))
  ==> (x1 (funcall f x1 x2) (funcall f (funcall f x1 x2) x2) ...)

  (scan* f (list x1 ... x_n-1 x_n) :initial-value z :from-end t)
  ==> (... (funcall f x_n-1 (funcall f x_n z)) (funcall f x_n z) z)

  (scan* f (list x1 ... x_n-1 x_n) :from-end t)
  ==> (... (funcall f x_n-1 (funcall f x_n-1 x_n)) (funcall f x_n-1 x_n) x_n)

  Examples,

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
  (10 9 7 4)"
  (when (and (functionp function)
             (if ivp (listp list) (consp list)))
    (if from-end
        (scan-right* function list key initial-value ivp)
        (scan-left* function list key initial-value ivp))))

(defun intersperse (element list)
  "Returns a list where ELEMENT is interspersed between the elements
of LIST.

  For example,

  INCF-CL> (intersperse 'x (replicate 3 'z))
  (Z X Z X Z)"
  (when (consp list)
    (let ((result (cons nil nil)))
      (do ((list list (rest list))
           (splice result
                   (cddr (rplacd splice (cons (first list)
                                              (cons element nil))))))
          ((null (rest list))
           (rplacd splice (cons (first list) nil))
           (rest result))))))

(defun nintersperse (element list)
  "Destructive version of INTERSPERSE."
  (when (listp list)
    (do ((xs list (let ((newcdr (cons element (rest xs))))
                    (cddr (rplacd xs newcdr)))))
        ((null (cdr xs)) list))))

(defun group (list &key (test #'eql))
  "Returns a list of lists where every item in each sublist satisfies
TEST and the concatenation of the result is equal to LIST.

  For example,

  INCF-CL> (mapcar (curry #'concatenate 'string)
                   (group (coerce \"Mississippi\" 'list)))
  (\"M\" \"i\" \"ss\" \"i\" \"ss\" \"i\" \"pp\" \"i\")"
  (when (and (listp list) (functionp test))
    (let* ((result (cons nil nil))
           (splice result))
      (do ()
          ((null list) (rest result))
        (destructuring-bind (x . xs) list
          (multiple-value-bind (ys zs) (span (curry test x) xs)
            (setf splice (rest (rplacd splice (list (cons x ys)))))
            (setf list zs)))))))
