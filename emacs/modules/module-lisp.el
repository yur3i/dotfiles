(pkg sly)
(setq inferior-lisp-program "sbcl")
(define-key lisp-mode-map (kbd "M-k") 'kill-sexp)
(define-key emacs-lisp-mode-map (kbd "M-k") 'kill-sexp)

(provide 'module-lisp)
