(in-package :entity-position)

(defvar *pos-to-obj* (make-weak-hash-table :test 'equalp :weakness :value))

(defstruct (vector2i (:constructor vector2i (x y)))
  (x 0 :type (signed-byte 64))
  (y 0 :type (signed-byte 64))
  (:documentation "Two dimentional 64 bit integer valued vector"))

(defmethod to-string (object)
  (print object nil))

(defmethod to-string ((object vector2i))
  (format nil "[~d, ~d]" (vector2i-x object) (vector2i-y object)))

(defentity has-position ()
  ((position :type vector2i
             :initarg :position
             :initform (vector2i 0 0)
             :reader get-position))
  (:documentation "An entity with a position value represented by a
  VECTOR2I"))

(define-condition position-blocked (serious-condition)
  ((position :type vector2i
             :initarg :position
             :reader get-position))
  (:report (lambda (c s)
             (format s "The position ~s is blocked." (to-string (get-position c)))))
  (:documentation "An action cannot be completed because a game world
position is somehow blocked."))

(define-condition position-occupied-for-move (position-blocked)
  ((occupant :type has-position
             :initarg :occupant
             :reader occupant)
   (mover :type has-position
          :initarg :mover
          :reader mover))
  (:report (lambda (c s)
            (format s "~s attempted to move to position ~s, but that
            position is already occupied by ~s."
                    (to-string (mover c))
                    (to-string (get-position c))
                    (to-string (occupant c))))))

(define-condition position-occupied-for-initialization (position-occupied-for-move)
  ((mover :initarg :initialize))
  (:report (lambda (c s)
            (format s "~s could not be created at position ~s because that
            position is already occupied by ~s."
                    (to-string (mover c))
                    (to-string (get-position c))
                    (to-string (occupant c))))))

(defmethod initialize-instance :after ((object has-position) &key)
  (check-type *pos-to-obj* hash-table)
  (let ((occupant (gethash (get-position object) *pos-to-obj*)))
    (if occupant
        (error 'position-occupied-for-initialization
               :position (get-position object)
               :occupant occupant
               :initialize object))
    (setf (gethash (get-position object) *pos-to-obj*) object)))

(defgeneric (setf get-position) (position entity)
  (:documentation "Set the position of the ENTITY to the value of
  POSITION"))

(defmethod (setf get-position) (position (entity has-position))
  (check-type *pos-to-obj* hash-table)
  (check-type position vector2i)
  (let ((occupant (gethash position *pos-to-obj*)))
    (if occupant
        (error 'position-occupied-for-move
                        :position position
                        :occupant occupant
                        :mover entity)
        (progn
          (remhash (get-position entity) *pos-to-obj*)
          (setf (gethash position *pos-to-obj*) entity))))
  (setf (slot-value entity 'position) position))

(defun get-entity-at-postion (position)
  "Return the entity with position slot value given by POSITION. A
result of NIL means there is none."
  (check-type *pos-to-obj* hash-table)
  (check-type position vector2i)
  (gethash position *pos-to-obj*))
