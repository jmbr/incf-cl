
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

(defpackage #:com.superadditive.incf-cl-system
  (:use :common-lisp :asdf))

(in-package #:com.superadditive.incf-cl-system)

(defsystem "incf-cl"
  :description "incf-cl is a library of convenience functions for Common Lisp"
  :version "0.0.1"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :license "X11"
  :components ((:doc-file "README")
               (:doc-file "THANKS")
               (:file "package")
               (:file "curry" :depends-on ("package"))
               (:file "vector" :depends-on ("package"))
               (:file "assemble" :depends-on ("package"))
               (:file "range" :depends-on ("package"))
               (:file "prelude" :depends-on ("package"))
               (:file "hash-table" :depends-on ("package"))
               (:file "sequence" :depends-on ("package"))
               (:file "iteration" :depends-on ("package"))))
