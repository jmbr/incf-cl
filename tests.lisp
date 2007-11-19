
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

(defpackage :com.superadditive.incf-cl-tests
  (:nicknames :incf-cl-tests)
  (:use :common-lisp :incf-cl :stefil)
  (:export #:test))

(in-package :com.superadditive.incf-cl-tests)

(defsuite test)

(in-suite test)

(deftest test-drop ()
  (is (eq nil (drop 0 nil)))
  (is (eq nil (drop 1 nil)))
  (is (eq nil (drop -1 (list 1 2 3))))
  (is (equal (list 1 2 3) (drop 0 (list 1 2 3))))
  (is (equal nil (drop 3 (list 1 2 3)))))

(deftest test-drop-while ()
  (is (eq nil (drop-while (constantly t) nil)))
  (is (equal (list 1 2 3) (drop-while (constantly nil)
                                      (list 1 2 3)))))

(deftest test-flip ()
  (is (= (funcall (flip #'-) 1 2) 1))
  (is (= (funcall (flip #'-) 2 1) -1)))

(deftest test-replicate ()
  (is (eq nil (replicate -1 'x)))
  (is (eq nil (replicate 0 'x)))
  (is (equal (list 1 1 1) (replicate 3 1))))

(deftest test-starts-with ()
  (is (eq t (starts-with "Hello, world!" "Hell")))
  (is (eq nil (starts-with "Hello, world!" "Hola"))))

(deftest test-ends-with ()
  (is (eq t (ends-with "Hello, world!" "world!")))
  (is (eq nil (ends-with "Hello, world!" "world"))))

(deftest test-assemble ()
  (is (eq nil (assemble (constantly nil))))
  (is (equal (list 1 2 3) (assemble x (<- x (range 1 3)))))
  (is (equal (acons 0 0 (acons 0 1 (acons 1 0 (acons 1 1 nil))))
             (assemble (cons i j) (<- i '(0 1)) (<- j '(0 1))))))

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

(deftest test-filter ()
  (is (equal (multiple-value-list (filter nil nil))
             (list nil nil)))
  (is (equal (multiple-value-list (filter (constantly t) (list 1 2 3)))
             (list (list 1 2 3) nil)))
  (is (equal (multiple-value-list (filter (constantly nil) (list 1 2 3)))
             (list nil (list 1 2 3))))
  (is (equal (multiple-value-list (filter #'oddp (list 1 2 3 4 5)))
             (list (list 1 3 5) (list 2 4)))))

;;; (incf-cl-test-suite)
