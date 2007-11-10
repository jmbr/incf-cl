(in-package :com.superadditive.incf-cl)

;;; Copyright (c) 2007 Juan M. Bello Rivas <jmbr@superadditive.com>
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(defun vector+ (v1 v2)
  "Returns the sum of the two vectors V1 and V2."
  (map 'vector #'+ v1 v2))

(defun vector- (v1 v2)
  "Returns the difference of the two vectors V1 and V2."
  (map 'vector #'- v1 v2))

(defun vector-zero-p (v)
  "Returns T if every compoment in V is zero."
  (every #'zerop v))

(defun vector= (v1 v2)
  "Returns T if both vectors V1 and V2 have the same components, NIL
otherwise."
  (equalp v1 v2))

(defun vector> (v1 v2)
  "Returns T if every component in V1 is greater than the
corresponding component in V2, NIL otherwise."
  (notany #'zerop (vector- v1 v2)))
