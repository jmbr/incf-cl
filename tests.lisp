
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

;;; To run test coverage use:
;;; 
;;; (progn
;;;   (require :sb-cover)
;;;   (declaim (optimize sb-cover:store-coverage-data))
;;;   (asdf:oos 'asdf:load-op :incf-cl :force t)
;;;   (incf-cl-tests:test)
;;;   (declaim (optimize (sb-cover:store-coverage-data 0)))
;;;   (sb-cover:report "/home/jmbr/projects/incf-cl/coverage-report/"))

(defpackage :com.superadditive.incf-cl-tests
  (:nicknames :incf-cl-tests)
  (:use :common-lisp :incf-cl :stefil)
  (:export #:test))

(in-package :com.superadditive.incf-cl-tests)

(in-root-suite)

(in-suite (defsuite test))

(deftest test-range ()
  (signals error (range 1 -1))
  (signals error (range 1 -1 2))
  (is (equal (range 1 5) (list 1 2 3 4 5)))
  (is (equal (range 1 2 5) (list 1 3 5))))

(deftest test-drop ()
  (is (eq nil (drop 0 nil)))
  (is (eq nil (drop 1 nil)))
  (is (eq nil (drop -1 (list 1 2 3))))
  (is (equal (list 1 2 3) (drop 0 (list 1 2 3))))
  (is (equal nil (drop 3 (list 1 2 3))))
  (is (equal nil (drop 100 (list 1 2 3))))
  (is (equal (list 3) (drop 2 (list 1 2 3)))))

(deftest test-take ()
  (is (eq nil (take 0 nil)))
  (is (eq nil (take 1 nil)))
  (is (eq nil (take -1 (list 1 2 3))))
  (is (eq nil (take 0 (list 1 2 3))))
  (is (equal (list 1 2 3) (take 3 (list 1 2 3))))
  (is (equal (list 1 2 3) (take 100 (list 1 2 3))))
  (is (equal (list 1 2) (take 2 (list 1 2 3)))))

(deftest test-take-while ()
  (is (eq nil (take-while (constantly t) nil)))
  (is (eq nil (take-while (constantly nil) (list 1 2 3))))
  (is (equal (list 1 2 3) (take-while (constantly t) (list 1 2 3))))
  (is (equal (list 1 2) (take-while (slice #'<= _ 2)
                                    (list 1 2 3)))))

(deftest test-drop-while ()
  (is (eq nil (drop-while (constantly t) nil)))
  (is (eq nil (drop-while (constantly t) (list 1 2 3))))
  (is (equal (list 1 2 3) (drop-while (constantly nil)
                                      (list 1 2 3))))
  (is (equal (list 3) (drop-while (slice #'<= _ 2)
                                  (list 1 2 3)))))

(deftest test-partition ()
  (is (eq nil (partition (constantly t) nil)))
  (signals type-error (partition (constantly t) nil :key 0))
  (is (equal (multiple-value-list (partition (constantly t) (list 1 2 3)))
             (list (list 1 2 3) nil)))
  (is (equal (multiple-value-list (partition (constantly nil) (list 1 2 3)))
             (list nil (list 1 2 3))))
  (is (equal (multiple-value-list (partition #'oddp (list 1 2 3 4 5)))
             (list (list 1 3 5) (list 2 4))))
  (is (equal (multiple-value-list (partition (slice #'= 3)
                                             (list "foo" "bar" "baz" "quux")
                                             :key #'length))
             '(("foo" "bar" "baz") ("quux")))))

(deftest test-flip ()
  (is (= (funcall (flip #'-) 1 2) 1))
  (is (= (funcall (flip #'-) 2 1) -1)))

(deftest test-replicate ()
  (is (eq nil (replicate -1 'x)))
  (is (eq nil (replicate 0 'x)))
  (is (equal (list 1 1 1) (replicate 3 1))))

(deftest test-span ()
  (let ((xs (list 1 1 1 3 5 6 8)))
   (is (eq (span (constantly t) nil) nil))
   (is (eq (span (constantly nil) xs) nil))
   (is (equal (span (constantly t) xs) xs))
   (multiple-value-bind (x y) (span #'oddp xs)
     (is (equal x (list 1 1 1 3 5)))
     (is (equal y (list 6 8))))))

(deftest test-break* ()
  (multiple-value-bind (x y) (break* #'oddp (nreverse (list 1 1 1 3 5 6 8)))
    (is (equal x (list 8 6)))
    (is (equal y (nreverse (list 1 1 1 3 5))))))

(deftest test-split-at ()
  (is (eq nil (split-at -10 (list 1 2 3))))
  (is (eq nil (split-at 0 nil)))
  (multiple-value-bind (x y) (split-at 0 (list 1 2 3))
    (is (and (equal x nil)
             (equal y (list 1 2 3)))))
  (is (equal (split-at 100 (list 1 2 3)) (list 1 2 3)))
  (multiple-value-bind (x y) (split-at 1 (list 1 2 3))
    (is (and (equal x (list 1))
             (equal y (list 2 3))))))

(deftest test-insert ()
  (signals type-error (insert nil nil :test nil))
  (signals type-error (insert 'x nil :test 0))
  (signals type-error (insert 'x nil :test nil :test-not nil))
  (is (equal (list 1 2 3) (insert 2 (list 1 3) :test-not #'>)))
  (is (equal (range 1 10)
             (let ((xs (list 3 6 10 7 4 1 8 2 9 5))
                   (ys nil))
               (dolist (x xs ys)
                 (setf ys (insert x ys)))))))

(deftest test-starts-with ()
  (signals simple-error (starts-with nil nil))
  (signals simple-error (starts-with "a" "abc"))
  (is (eq t (starts-with "Hello, world!" "Hell")))
  (is (eq nil (starts-with "Hello, world!" "Hola"))))

(deftest test-ends-with ()
  (signals simple-error (ends-with nil nil))
  (signals simple-error (ends-with "a" "abc"))
  (is (eq t (ends-with "Hello, world!" "world!")))
  (is (eq nil (ends-with "Hello, world!" "world"))))

(deftest test-lc ()
  (is (eq nil (lc (constantly nil))))
  (is (equal (list 1 2 3) (lc x (<- x (range 1 3)))))
  (is (equal (acons 0 0 (acons 0 1 (acons 1 0 (acons 1 1 nil))))
             (lc (cons i j) (<- i '(0 1)) (<- j '(0 1))))))

(deftest test-hash-table->alist ()
  (is (equal (sort (hash-table->alist
                    (alist->hash-table
                     (pairlis '("one" "two" "three")
                              '(1 2 3))))
                   #'< :key #'cdr)
             (acons "one" 1 (acons "two" 2 (acons "three" 3 nil))))))

(deftest test-string-join ()
  (is (eq nil (string-join nil nil)))
  (is (eq nil (string-join nil)))
  (is (eq nil (string-join nil ", ")))
  (is (string= "Hello world"
               (string-join (list "Hello" "world"))))
  (is (string= "Hello, world"
               (string-join (list "Hello" "world") ", "))))

(defun read-lines (stream)
  (when (streamp stream)
    (unfold (slice #'eq stream _)
            #'identity
            (lambda (x)
              (declare (ignore x))
              (read-line stream nil stream))
            (read-line stream nil stream))))

(deftest test-unfold ()
  (is (eq nil (unfold (constantly t) #'identity #'identity 0)))
  (is (equal (range 0 .1 (/ pi 2))
             (unfold (slice #'> _ (/ pi 2))
                     #'identity
                     (lambda (x) (+ x 0.1))
                     0)))
  (with-input-from-string (stream
                           "1
                                     2
                                     3")
    (is (equal (list "1" "2" "3")
               (mapcar (slice #'string-trim " #\Tab" _)
                       (read-lines stream))))))

(deftest test-doctest ()
  "This is a sample docstring.

  INCF-CL-TESTS> \"Hello, doctest!\"
  \"Hello, doctest!\"

  INCF-CL-TESTS> (unzip (pairlis (list 'a 'b 'c) (list 1 2 3)))
  (A B C)
  (1 2 3)

  INCF-CL-TESTS> (unzip 0)
  NIL

  INCF-CL-TESTS> (signals-p cell-error (error 'division-by-zero))
  NIL

  INCF-CL-TESTS> (signals-p division-by-zero (error 'division-by-zero))
  T"
  (is (eq t (doctest :incf-cl-tests :function 'test-doctest)))
  ;; Here we run doctests for the whole package.
  (is (eq t (doctest :incf-cl))))

(defun random-list (n m)
  "Returns a list of N elements taken uniformly from the positive
 integers less than M."
  (let (list)
    (dotimes (i n list)
      (push (random m) list))))

(deftest test-scan* ()
  (let ((zs (random-list 100 100)))
    (is (eql (first (last (scan* #'max zs :initial-value 5)))
             (reduce #'max zs :initial-value 5))))
  (is (eql (first (scan* #'+ (coerce "Hello, world!" 'list)
                         :key #'char-code :from-end t))
           (reduce #'+ "Hello, world!" :key #'char-code))))

(deftest test-cycle ()
  (is (eq nil (cycle nil)))
  (is (equal (list 0 1 0 1) (take 4 (cycle (list 0 1))))))

(deftest test-ncycle ()
  (is (eq nil (ncycle nil)))
  (is (equal (list 0 1 0 1) (take 4 (ncycle (list 0 1))))))

(deftest test-intersperse ()
  (is (eq nil (intersperse 0 nil)))
  (is (equal (list 'z) (intersperse 'x (list 'z))))
  (is (equal (list 'z 'x 'z) (intersperse 'x (list 'z 'z)))))

(deftest test-nintersperse ()
  (is (eq nil (nintersperse 0 nil)))
  (is (equal (list 'z) (nintersperse 'x (list 'z))))
  (is (equal (list 'z 'x 'z) (nintersperse 'x (list 'z 'z)))))

(deftest test-group ()
  (signals type-error (group nil :key 0))
  (signals type-error (group nil :test 0))
  (is (eq nil (group nil :test #'equal)))
  (is (equal (group (list "abc" "aardvark" "ant" "buffalo" "zebra") :key (slice #'elt _ 0))
             '(("abc" "aardvark" "ant") ("buffalo") ("zebra"))))
  (is (equal (concatenate 'string
                          (mapcan #'identity
                                  (group (coerce "Mississippi" 'list))))
             "Mississippi")))

(deftest test-$ ()
  (is (eq 5 (funcall ($ #'1+ #'1+ #'1+ #'1+ #'1+) 0))))

(deftest test-fixed-point ()
  (is (eql 0 (fixed-point #'identity 0 :max-steps -1)))
  (is (eql 1 (fixed-point #'1+ 0 :max-steps 1))))
