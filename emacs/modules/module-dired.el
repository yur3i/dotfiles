(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "a") 'bongo-dired-append-enqueue-lines)

(provide 'module-dired)
