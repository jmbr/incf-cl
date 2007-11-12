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

(defun starts-with (sequence prefix &key (test #'equal))
  "Returns T if SEQUENCE starts with PREFIX, NIL otherwise.  PREFIX
can also be a list of prefixes to try to match."
  (assert (and sequence prefix))
  (flet ((match-start (sequence prefix)
           (let ((sequence-len (length sequence))
                 (prefix-len (length prefix)))
             (assert (<= prefix-len sequence-len))
             (funcall test (subseq sequence 0 prefix-len) prefix))))
    (if (listp prefix)
        (dolist (p prefix)
          (when (match-start sequence p)
            (return t)))
        (match-start sequence prefix))))

(defun ends-with (sequence suffix &key (test #'equal))
  "Returns T if SEQUENCE ends with SUFFIX, NIL otherwise.  SUFFIX can
also be a list of suffixes to try to match."
  (assert (and sequence suffix))
  (flet ((match-end (sequence suffix)
           (let ((sequence-len (length sequence))
                 (suffix-len (length suffix)))
             (assert (<= suffix-len sequence-len))
             (funcall test (subseq sequence
                                   (- sequence-len suffix-len)
                                   sequence-len) suffix))))
    (if (listp suffix)
        (dolist (s suffix)
          (when (match-end sequence s)
            (return t)))
        (match-end sequence suffix))))
