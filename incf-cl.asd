
;;; Copyright (c) 2007-2010 Juan M. Bello Rivas <jmbr@superadditive.com>
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

(cl:defpackage #:incf-cl-system
  (:use #:common-lisp #:asdf))

(cl:in-package #:incf-cl-system)

(defsystem #:incf-cl
  :description "INCF CL is a library of convenience functions for Common Lisp"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :licence "X11"
  :depends-on (#:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "function")
               (:file "lc")
               (:file "nest")
               (:file "unfold")
               (:file "range")
               (:file "prelude")
               (:file "unscan")
               (:file "fixed-point")
               (:file "hash-table")
               (:file "iteration")
               (:file "string")
               (:file "doctest")
               (:file "symbol")))

(defsystem #:incf-cl-test
  :description "Test suite for the INCF CL library."
  :depends-on (#:incf-cl #:hu.dwim.stefil #:hu.dwim.stefil+swank)
  :components ((:file "test-suite")))

(defmethod perform ((op test-op) (system (eql (find-system "incf-cl"))))
  (asdf:load-system "incf-cl-test")
  (eval (read-from-string "(hu.dwim.stefil:funcall-test-with-feedback-message 'incf-cl-test:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system "incf-cl"))))
  nil)

(delete-package *package*)
