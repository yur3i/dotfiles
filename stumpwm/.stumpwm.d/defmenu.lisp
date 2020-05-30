(in-package :stumpwm)

(defmacro defmenu (name key mnu-function choice-function)
  (let ((menu-name (intern1 (concat (string name) "-menu"))))
    `(progn
       (defcommand ,menu-name () ()
	 (labels ((pick (options)
		 (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
		   (cond
                    ((null selection)
                     (throw 'stumpwm::error "Abort."))
                    ((stringp (second selection))
                     (second selection))
                    (t
                     (pick (cdr selection)))))))
	  (let ((choice (pick ,mnu-function)))
	    ,choice-function)))

       (define-key *top-map* (kbd ,(concatenate 'string "s-" key))
         ,(string menu-name))
       )))
