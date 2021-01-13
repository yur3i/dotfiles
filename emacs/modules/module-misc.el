(delete-selection-mode 1)
(electric-pair-mode 1)

(show-paren-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))

      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default abbrev-mode t
	      scroll-preserve-screen-position t
	      scroll-conservatively 1
	      scroll-margin 0)

(defmacro mode-keys (map keys)
  (let ((keybindings (cl-loop for key in keys collect
			      `(define-key ,map (kbd ,(car key)) ,(cadr key)))))
    `,@keybindings))

(provide 'module-misc)
