(in-package :stumpwm)

;;; TODO has a bug if cmd is >1 word fails can't name command that way
(defmacro defprog (name key cmd props &key rat)
  "binds key to stumpwm command name that select window with props or runs cmd"
  (let ((name-focus (intern1 (concat (string name) "-FOCUS")))
        (name-pull (intern1 (concat "pull-" (string name)))))
    `(progn
       (shifting-command ,name ,props ,cmd ,rat)
       (define-key *root-map* (kbd ,key)
         ,(string name))

       (defcommand ,name-focus () ()
         (run-shell-command ,cmd)
         ;; (focus-matching-window ,props)
         )
       (define-key *root-map*
           (kbd ,(string-upcase key))
         ,(string name-focus))
              ))
  )
