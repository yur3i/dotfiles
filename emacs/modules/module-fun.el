(defun jump-to-char (arg char)
  "Jump to the next instance of a character"
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1)
  (forward-char))

(defun my-repeat ()
  "Repeats the last command, complex or otherwise without confirming"
  (interactive)
  (if (eq last-command (caar command-history))
      (repeat-complex-command-no-confirm)
    (call-interactively last-command))
  (setq this-command  last-command))

(defun surround-sexp ()
  "surround the current sexp with brackets"
  (interactive)
  (save-mark-and-excursion
   (up-list)
   (set-mark-command nil)
   (backward-sexp)
   (insert "(")
   (forward-sexp)
   (insert ")")))

(defun kill-whole-line ()
  "kills the entire line on which the cursor is located, and places the
cursor as close to its previous position as possible."
  (interactive)
  (progn
    (let ((y (current-column))
          (a (progn (beginning-of-line) (point)))
          (b (progn (forward-line 1) (point))))
      (kill-region a b)
      (move-to-column y))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-commandlocal    (downcase-region $p1 $p2)
      (put this-command 'state 0)))))))

(defun cut-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-whole-line)))

(defun rename1 (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defun move-point-forward-out-of-string ()
  (interactive)
  (while (current-quotes-char) (forward-char)))

(defun move-point-backward-out-of-string ()
  (interactive)  
  (while (current-quotes-char) (backward-char)))

(defun indent-or-hippie ()
  "If at the start of the line, indent else call hippie-expand"
  (interactive)
  (if (eql (current-column) 0)
      (indent-for-tab-command)
    (hippie-expand 1)))

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

(defun clear-dwim ()
  "Clear within the nearest delimiter"
  (interactive)
  (let ((odelims (list (string-to-char "\"")
		       (string-to-char "\'")
		       (string-to-char "(")
		       (string-to-char "[")
		       (string-to-char "{")))
	(cdelims (list (string-to-char "\"")
		       (string-to-char "\'")
		       (string-to-char ")")
		       (string-to-char "]")
		       (string-to-char "}"))))
    (while (not (member (char-after) cdelims))
      (delete-char 1))
    (let* ((pos (cl-position (char-after) cdelims))
	   (dlm (nth pos odelims)))
     (while (not (eql (char-before) dlm))
       (backward-delete-char 1)))))

(defun clear-to-end ()
  "Clear within the nearest delimiter"
  (interactive)
  (let ((odelims (list (string-to-char "\"")
		       (string-to-char "\'")
		       (string-to-char "(")
		       (string-to-char "[")
		       (string-to-char "{")))
	(cdelims (list (string-to-char "\"")
		       (string-to-char "\'")
		       (string-to-char ")")
		       (string-to-char "]")
		       (string-to-char "}"))))
    (while (not (member (char-after) cdelims))
      (delete-char 1))))

(provide 'module-fun)
