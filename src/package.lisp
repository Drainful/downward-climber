(uiop:define-package #:utilities
    (:shadowing-import-from :generic-cl :emptyp)
  (:use
   :generic-cl
   :alexandria)
  (:export
   :dofor))

(uiop:define-package #:superclass-map
    (:shadowing-import-from :generic-cl :emptyp)
  (:use
   :generic-cl
   :alexandria
   :utilities)
  (:export
   :make-superclass-map
   :update-superclass-map
   :add-object
   :remove-object
   :get-objects-of-class
   :get-objects-of-classes))

(uiop:define-package #:entity
    (:shadowing-import-from :generic-cl :emptyp)
  (:use
     :generic-cl
     :alexandria
     :superclass-map
     :utilities)
  (:export
   :defentity
   :apply-system
   :delete-entity
   :delete-all-entities))

(uiop:define-package #:entity-position
    (:use
     #:cl
     #:entity
     #:trivial-garbage)
  (:export
   :vector2i
   :has-position
   :get-position
   :get-entity-at-position))

(uiop:define-package #:downward-climber
    (:use
     #:cl
     #:entity
     #:trivial-garbage
     ;; #:cl #:cepl #:rtg-math #:vari
     ;; #:cepl.skitter
     ;; #:croatoan
     ;; #:livesupport
     )
  (:export :start :stop))
