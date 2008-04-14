(in-package :com.superadditive.incf-cl)

;;; Copyright (c) 2008 Juan M. Bello Rivas <jmbr@superadditive.com>
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

;;; An implementation of a bounded circular list.

(defclass circular-list ()
  ((initial
    :initform nil
    :accessor initial)
   (current
    :initform nil
    :accessor current)
   (circular-list-bound
    :initform 0
    :initarg :bound
    :reader circular-list-bound)
   (circular-list-length
    :initform 0
    :accessor circular-list-length)))


(defgeneric circular-list-first (circular-list))

(defgeneric circular-list-bound-reached-p (circular-list))

(defgeneric circular-list-add (circular-list elem))

(defgeneric circular-list->proper-list (circular-list))


(defmethod circular-list-first ((list circular-list))
  (when (plusp (circular-list-length list))
    (first (initial list))))

(defmethod circular-list-bound-reached-p ((list circular-list))
  (= (circular-list-length list) (circular-list-bound list)))

(defmethod circular-list-add ((list circular-list) elem)
  (assert (<= (circular-list-length list) (circular-list-bound list)))
  (if (circular-list-bound-reached-p list)
      (let ((new-cell (cons elem (rest (initial list)))))
        (setf (initial list) (rest (initial list))
              (rest (current list)) new-cell
              (current list) new-cell))
      (let ((new-cell (cons elem (initial list))))
        (if (zerop (circular-list-length list))
            (setf (initial list) new-cell
                  (current list) new-cell)
            (setf (rest (current list)) new-cell
                  (current list) new-cell))
        (incf (circular-list-length list))))
  list)

(defmethod circular-list->proper-list ((list circular-list))
  (when (plusp (circular-list-length list))
    (take (circular-list-bound list) (initial list))))
