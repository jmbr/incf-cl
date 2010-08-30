(cl:in-package #:incf-cl)

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

(defun string-join (list &optional (sep " "))
  "Returns a string joining each string in LIST by SEP.  If SEP is not
specified, the default separator is a space."
  ;; TODO This should use intersperse as soon as it works with
  ;; arbitrary sequences.

  ;; Based on Pascal Costanza's approach at
  ;; http://groups.google.com/group/comp.lang.lisp/msg/8adbef3a067ece78
  (when list
    (with-output-to-string (stream)
      (write-string (first list) stream)
      (dolist (string (rest list) stream)
        (write-string sep stream)
        (write-string string stream)))))
