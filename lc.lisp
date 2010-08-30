(in-package :com.superadditive.incf-cl)

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

;;; This code has been adapted from the paper Simple and Efficient
;;; Compilation of List Comprehension in Common Lisp by Mario
;;; Latendresse.

(defmacro lc (collection-form &rest quantifiers)
  "Assembles a multiset containing the results of evaluating
COLLECTION-FORM and subject to QUANTIFIERS."
  (if quantifiers
      `(cl:loop :repeat 1
                ,@(translate-comprehension collection-form quantifiers))
      (error "No quantifiers specified.")))

(defun translate-comprehension (collection-form quantifiers)
  (if (consp quantifiers)
      (translate-generator-or-filter collection-form quantifiers)
      `(:collect ,collection-form)))

(defun translate-generator-or-filter (collection-form quantifiers)
  (flet ((generator-p (z)
           (and (consp z) (eq (first z) '<-) (= (length z) 3))))
    (destructuring-bind (q . qr) quantifiers
      (if (generator-p q)
          (translate-generator collection-form (second q) (third q) qr)
          (translate-filter collection-form q qr)))))

(defun translate-generator (collection-form variable collection quantifiers)
  `(:nconc (cl:loop :for ,variable :in ,collection
                    ,@(translate-comprehension collection-form quantifiers))))

(defun translate-filter (collection-form filter-form quantifiers)
  `(:when ,filter-form
     ,@(translate-comprehension collection-form quantifiers)))
