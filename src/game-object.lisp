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

(defun apply-system (function &rest superclass-list)
  "Apply function to each permutation of game objects which fulfill
each superclass-list in order and collect the results in a list.
ex. 
    ;;call APPLY-BULLET-DAMAGE on every pair of bullet and target
    (apply-system #'apply-bullet-damage 
                  '(bullet) '(has-hitbox has-health)) 
    ;;call RESOLVE-COLLISIONS on every ordered pair of game
    ;;objects which have hitboxes and are solid
    (apply-system #'resolve-collisions 
                  '(has-hitbox solid) '(has-hitbox solid))") ; TODO
