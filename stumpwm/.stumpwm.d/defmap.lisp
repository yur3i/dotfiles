(in-package :stumpwm)

(defmacro defmap (name key binds)
  (let ((name-map (intern1 (concat (string name) "-map"))))
    `(progn
       (defvar ,name
	 (let ((y (stumpwm:make-sparse-keymap)))
	   (stumpwm:define-key y (stumpwm:kbd ,(car (car binds))) ,(cadr (car binds)))
	   y))
       (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd ,key) ,name)
       )))


