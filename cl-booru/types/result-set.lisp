(in-package :cl-booru)

(defclass <result-set> ()
  ((images :initform nil :initarg :images :accessor <result-set>-images)
   (count  :initform 0   :initarg :count  :accessor <result-set>-count)
   (total  :initform 0   :initarg :total  :accessor <result-set>-total)
   (offset :initform 0   :initarg :offset :accessor <result-set>-offset)))
