
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

(cl:in-package :cl-user)

(cl:defpackage :com.superadditive.incf-cl
  (:nicknames :incf-cl)
  (:use #:common-lisp)
  (:export #:vector+
           #:vector-
           #:vector*
           #:vector>
           #:vector=
           #:vector-zero-p
           #:lc
           #:assemble
           #:<-
           #:unfold
           #:unfold-right
           #:range
           #:break*
           #:cycle
           #:ncycle
           #:drop
           #:drop-while
           #:fixed-point
           #:flip
           #:group
           #:insert
           #:partition
           #:replicate
           #:span
           #:split-at
           #:take
           #:take-while
           #:unscan
           #:unzip
           #:scan*
           #:intersperse
           #:nintersperse
           #-clisp #:dohash
           #:hash-table->alist
           #:alist->hash-table
           #:starts-with
           #:ends-with
           #:while
           #:string-join
           #:signals-p
           #:doctest
           #:slice
           #:_
           #:$))

(cl:pushnew :incf-cl *features*)
