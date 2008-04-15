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

;;; An implementation of a bounded circular list (aka ring).

(defclass ring ()
  ((initial
    :initform nil
    :accessor initial)
   (current
    :initform nil
    :accessor current)
   (ring-bound
    :initform 0
    :initarg :bound
    :reader ring-bound)
   (ring-length
    :initform 0
    :accessor ring-length)))


(defgeneric ring-first (ring))

(defgeneric ring-add (ring elem))

(defgeneric ring->list (ring))


(defun make-ring (bound &optional initial-values)
  (let ((list (make-instance 'ring :bound bound)))
    (dolist (x initial-values list)
      (ring-add list x))))

(defmethod ring-first ((list ring))
  (when (plusp (ring-length list))
    (first (initial list))))

(defmethod ring-add ((list ring) elem)
  (assert (<= (ring-length list) (ring-bound list)))
  (if (= (ring-length list) (ring-bound list))
      (let ((new-cell (cons elem (rest (initial list)))))
        (setf (initial list) (rest (initial list))
              (rest (current list)) new-cell
              (current list) new-cell))
      (let ((new-cell (cons elem (initial list))))
        (if (zerop (ring-length list))
            (setf (initial list) new-cell
                  (current list) new-cell)
            (setf (rest (current list)) new-cell
                  (current list) new-cell))
        (incf (ring-length list))))
  list)

(defmethod ring->list ((list ring))
  (when (plusp (ring-length list))
    (take (ring-bound list) (initial list))))

