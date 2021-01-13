(defun make-ref ()
  (interactive)
  (when (use-region-p)
    (let ((region (car (read-from-string (buffer-substring (mark) (point))))))
      (delete-region (mark) (point))
      (insert (concat "@" (car region) "{" (cadr region) ","))
      (cl-loop for entry in (cddr region)
	       do
	       (insert (concat "\n" (car entry) " = " "{" (cadr entry) "},")))
      (insert "\n} "))))

(provide 'module-tex)

@article{ting,





