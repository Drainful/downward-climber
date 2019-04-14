(defsystem #:downward-climber
  :description "Roguelike attempt #2"
  :author "Adrian Fullmer"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:rtg-math.vari #:cepl.sdl2 #:slynk #:livesupport #:cepl.skitter.sdl2 #:dirt
                      #:croatoan
                      #:closer-mop
                      #:trivial-garbage
                      (:version #:generic-cl "0.2"))
  :in-order-to ((test-op (test-op :downward-climber-test)))
  :pathname "src"
  :components ((:file "package")
               (:file "utilities")
               (:file "superclass-map")
               (:file "entity")
               (:file "entity-position")
               (:file "view")
               (:file "downward-climber")
               ;; (:file "cepl-test")
               ))

(defsystem #:downward-climber-test
  :description "tests for downward-climber"
  :author "Adrian Fullmer"
  :depends-on (#:downward-climber
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :components ((:test-file "superclass-map"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
