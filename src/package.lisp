(uiop:define-package #:utilities
    (:shadowing-import-from :generic-cl :emptyp)
  (:use
   :generic-cl
   :alexandria)
  (:export :dofor))

(uiop:define-package #:superclass-map
    (:shadowing-import-from :generic-cl :emptyp)
  (:use
   :generic-cl
   :alexandria
   :utilities)
  (:export
   :make-superclass-map
   :add-object
   :remove-object
   :get-objects-of-class
   :get-objects-of-classes))

(uiop:define-package #:game-object
    (:shadowing-import-from :generic-cl :emptyp)
    (:use
     :generic-cl
     :alexandria
     :superclass-map
     :utilities)
  (:export :defclass-gameobj))

(uiop:define-package #:downward-climber
    (:use
     #:cl #:cepl #:rtg-math #:vari
     :cepl.skitter
     :livesupport)
  (:export :start :stop))
