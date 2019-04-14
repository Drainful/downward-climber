(in-package superclass-map)

(defun get-superclasses (object)
  "Get all superclasses of a CLOS object"
  (class-precedence-list (class-of object)))

(defstruct (superclass-map
            (:constructor make-superclass-map ())
            (:copier nil))
  "Maps superclasses of objects to sets of objects. Note that the map
is not updated in the case that a class is redefined (for example to
have additional superclasses)."
  (inner-hash-map (make-hash-map)))

(defun inner (superclass-map)
  "Get the inner hash-map of the superclass map"
  (check-type superclass-map superclass-map)
  (superclass-map-inner-hash-map superclass-map))

(defun add-object (object superclass-map)
  "Add OBJECT to the superclass map given by SUPERCLASS-MAP. The
object must be equalp-able because it is inserted into a hash set"
  (check-type superclass-map superclass-map)
  (dolist (superclass (get-superclasses object))
    (setf (get (class-name superclass)
               (inner superclass-map))
          (nadjoin object (ensure-get (class-name superclass)
                                      (inner superclass-map)
                                      (make-hash-set))))))

(defun remove-object (object superclass-map)
  "Remove OBJECT from the superclass map given by SUPERCLASS MAP"
  (check-type superclass-map superclass-map)
  (dolist (superclass (get-superclasses object))
    (let ((set (get (class-name superclass) (inner superclass-map))))
      (when set
        (erase set object)))))

(defun get-objects-of-class (superclass superclass-map)
  "Get a HASH-SET of objects contained within SUPERCLASS-MAP which
have the class SUPERCLASS in their CLASS-PRECEDENCE-LIST. SUPERCLASS
may also be a symbol which names a class, or T, signifying all
classes."
  (check-type superclass-map superclass-map)
  (get (if (symbolp superclass)
           superclass
           (class-name superclass))
       (inner superclass-map)))

(defun get-objects-of-classes (superclasses superclass-map)
  "Like GET-OBJECTS-OF-CLASS except that a list of classes are
provided, and the intersection of the given sets is found."
  (check-type superclasses list)
  (check-type superclass-map superclass-map)
  (reduce #'intersection
          (mapcar (lambda (superclass)
                    (get-objects-of-class superclass superclass-map))
                  superclasses)))

(defun update-superclass-map (class superclass-map)
  "Update a superclass map for a given class (not class symbol) so that it (the map) contains the
   correct objects for each class"

  (let ((removed-objects nil)) ; create a list to contain removed objects
    ;; iterate over all objects in the superclass-map
    (dofor (class-and-objects (inner superclass-map)) ; for each superclass in the map
      (dofor (object (cdr class-and-objects)) ; for each object of that superlass
        (when (typep object class) ; if the object is of the given class
          (remove-object object superclass-map) ; remove the object from the superclass-map
          (setf removed-objects (cons object removed-objects))))) ; add the object to the removed objects list

    ;; for each removed object, add it to the superclass map again so
    ;; that its superclasses may be refreshed.
    (dolist (removed-object removed-objects)
      (add-object removed-object superclass-map))))

;; want to get object generic printing working
(defun print-superclass-map (superclass-map &optional (stream t))
  (dofor (class-and-objects (inner superclass-map))
    (when (not (emptyp (cdr class-and-objects)))
      (if (eql (car class-and-objects) t)
          (format stream "ALL: ~%")
          (format stream "CLASS ~A: ~%" (car class-and-objects)))
      (dofor (object (cdr class-and-objects))
        (format stream "~A ~%" object))))
  superclass-map)
