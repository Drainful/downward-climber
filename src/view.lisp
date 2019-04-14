(in-package :downward-climber)

;; (defentity )

;; Replace with a call to apply-system when that is written

;; (defun view (map scr &optional (game-objects *game-object-superclass-map*))
;;   (clear scr)
;;   (let* ((width (min (.width scr) (array-dimension map 0)))
;;          (height (min (.height scr) (array-dimension map 1))))
;;     (loop for x from 0 below width do
;;          (loop for y from 0 below height do
;;               (move scr y x)
;;               (format scr (if (aref map x y)
;;                               "."
;;                               " "))))
;;     (dolist (has-pos (set-to-list (get-game-objects-of-class (find-class 'has-pos) (get-game-objects model))))
;;       (draw-object drawable scr)))
;;   (refresh scr))

;; (defgeneric draw-object (drawable screen)
;;   (:documentation "draws a drawable object on the screen"))
