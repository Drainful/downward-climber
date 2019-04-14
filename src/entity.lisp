(in-package :entity)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *entity-superclass-map* (make-superclass-map)))

(defvar *id-counter* 0 "The id of the next game object that will be created")

(defun print-entities (&optional (stream t))
  (superclass-map::print-superclass-map *entity-superclass-map* stream))

(defclass entity ()
  ((id :type (unsigned-byte 64))))

(defmethod equalp ((entity-a entity) (entity-b entity))
  (= (slot-value entity-a 'id)
     (slot-value entity-b 'id)))

(defmethod initialize-instance :after ((object entity) &key)
  (setf (slot-value object 'id) *id-counter*)
  (incf *id-counter*)
  (add-entity object))

(defun add-entity (entity)
  (add-object entity *entity-superclass-map*))

(defun get-entities-of-class (superclass)
  (get-objects-of-class superclass *entity-superclass-map*))

(defun get-entities-of-classes (superclasses)
  (get-objects-of-classes superclasses *entity-superclass-map*))

(defmacro defentity (name direct-superclasses direct-slots &rest options)
  "Wrapper around CL:DEFCLASS, updates game object superclass map when
called."
  (let ((already-is-entity (some #'identity
                                 (mapcar (lambda (superclass)
                                           (closer-mop:subclassp (find-class superclass)
                                                                 (find-class 'entity)))
                                         direct-superclasses))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (update-superclass-map (defclass ,name (,@(unless already-is-entity `(entity))
                                               ,@direct-superclasses)
                                (,@direct-slots)
                                ,@options)
                              *entity-superclass-map*))))

(defun delete-entity (entity)
  (check-type entity entity)
  (remove-object entity *entity-superclass-map*))

(defun delete-all-entities ()
  (setq *entity-superclass-map* (make-superclass-map))
  (setq *id-counter* 0))

(defmacro apply-system (function &rest superclass-lists)
  "Apply function to each permutation of game objects which fulfill
each superclass-list in order

EXAMPLES: 

    ;;call APPLY-BULLET-DAMAGE on every pair of bullet and target
    (defun apply-bullet-damage (bullet target)
       ...)
    (apply-system #'apply-bullet-damage 
                  '(bullet) '(has-hitbox has-health))"
  (let* (;; This is a list of the symbols for objects
         (object-symbols (mapcar (lambda (_) (declare (ignore _)) (gensym)) superclass-lists))
         ;; Code to call the function on the objects.
         (expression `(funcall ,function ,@object-symbols))
         ;; Will represent a list of sets of game objects.
         (objects (gensym)))
    ;; Repeatedly wrap EXPRESSION with DOFORs over game objects,
    ;; binding each OBJECT-SYMBOL to elements of the appropriate set
    ;; of game objects.
    (dotimes (n (length object-symbols))
      (setq expression
            `(dofor (,(nth n object-symbols) (nth ,n ,objects))
               ,expression)))
    ;; Wrap the expression with code to create OBJECTS at runtime.
    (setq expression
          `(let ((,objects
                   (mapcar #'get-entities-of-classes
                           (list ,@(copy-list superclass-lists)))))
             ,expression))))
