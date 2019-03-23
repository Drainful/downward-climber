(in-package :game-object)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *game-object-superclass-map* (make-superclass-map)))

;; I want to be able to make an object which is tracked by a
;; superclass map upon creation, and whose superclass map is updated
;; when any of its superclasses is redefined to have different
;; superclasses.

(defmacro defclass-gameobj (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-superclass-map (defclass ,@body)
                            *game-object-superclass-map*)))

(defun get-game-objects-of-class (superclass)
  (get-objects-of-class superclass *game-object-superclass-map*))

(defun get-game-objects-of-classes (superclasses)
  (get-objects-of-classes superclasses *game-object-superclass-map*))
