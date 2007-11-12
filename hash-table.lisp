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

(defmacro dohash ((key value hash-table &optional (result-form nil))
                  &body body)
  "DOHASH iterates over the keys and values of HASH-TABLE.  It returns
NIL or the result of evaluating RESULT-FORM if it was specified."
  `(loop
      for ,key being each hash-key in ,hash-table using (hash-value ,value) do
      (progn
        ,@body)
      finally (return ,result-form)))

(defun hash-table->alist (hash-table)
  "Returns an association list containing the keys and values in
HASH-TABLE."
  (let (alist)
    (dohash (key value hash-table alist)
      (setf alist (acons key value alist)))))

(defun alist->hash-table (alist &rest make-hash-table-parameters)
  "Returns a new hash table containing the keys and values present in
each element of the association list ALIST.  The hash table will be
created using MAKE-HASH-TABLE-PARAMETERS."
  (let ((hash-table
         (apply #'make-hash-table make-hash-table-parameters)))
    (dolist (x alist hash-table)
      (setf (gethash (car x) hash-table) (cdr x)))))
