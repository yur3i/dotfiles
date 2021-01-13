(pkg geiser)
(setq geiser-active-implementations '(chez))
(define-key scheme-mode-map (kbd "M-k") 'kill-sexp)
(provide 'module-scm)
