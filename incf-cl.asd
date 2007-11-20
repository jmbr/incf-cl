
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

(defpackage :com.superadditive.incf-cl-system
  (:use :common-lisp :asdf))

(in-package :com.superadditive.incf-cl-system)

(defsystem "incf-cl"
  :description "incf-cl is a library of convenience functions for Common Lisp"
  :version "0.0.1"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :license "X11"
  :serial t
  :components ((:static-file "incf-cl.asd")
               (:doc-file "README")
               (:doc-file "THANKS")
               (:file "package")
               (:file "curry")
               (:file "vector")
               (:file "assemble")
               (:file "range")
               (:file "prelude")
               (:file "hash-table")
               (:file "sequence")
               (:file "iteration")
               (:file "string")))

(defsystem "incf-cl-tests"
  :description "Test suite for the INCF-CL library."
  :depends-on (:stefil :incf-cl)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (system (eql (find-system :incf-cl))))
  (operate 'load-op :incf-cl-tests)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'incf-cl-tests:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :incf-cl))))
  nil)
