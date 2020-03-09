;; packaging
(add-to-list 'load-path "/home/jorde/.emacs.d/lisp")
(require 'package)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
;; Theming
(use-package modus-vivendi-theme
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)) 
;; Smartparens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode))
;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets :ensure t)
  (yas-global-mode))
;; git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
;; org mode
(use-package org
  :ensure t
  :config
  (require 'org)
  (require 'ox-latex)
  (setq org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t)))
  (add-hook 'org-mode-hook 'visual-line-mode))
;; ivy, counsel, swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t
  :config
  (bind-keys
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))
(use-package swiper
  :ensure t
  :config
  (bind-keys
   ("C-s" . swiper)))
;; scheme
(use-package geiser
  :ensure t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-active-implementations '(chez))
  (add-hook 'geiser-mode-hook 'smartparens-mode))

;; prog mode specific keys
(bind-keys :map prog-mode-map
	   ("M-k" . kill-sexp))

;; global keys
(bind-keys
 ("M-]" . next-buffer)
 ("M-[" . previous-buffer)
 ("M-o" . other-window)
 ("M-u" . undo)
 ("C-x k" . kill-this-buffer)
 ("C-x K" . kill-buffer-and-window) 
 ("C-x 2" . (lambda () (interactive)(split-window-vertically) (other-window 1)))
 ("C-x 3" . (lambda () (interactive)(split-window-horizontally) (other-window 1))))

;; global modes
(delete-selection-mode 1)
(show-paren-mode 1)

;; custom functions
(defun run-python3.7 ()
    "Runs python 3.7"
  (interactive)
  (run-python "python3.7"))

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
(global-set-key (kbd "C-M-k") 'kill-whole-line)

(defun rename1 (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (completing-read "New name: " nil nil nil (buffer-name))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move1 (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)))))

(defun jump-to-char (arg char)
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1)
  (forward-char))
(global-set-key (kbd "C-z") 'jump-to-char)

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
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))
(global-set-key (kbd "M-c") 'toggle-letter-case)

(defun run-sqlite ()
  "Runs a sqlite shell"
  (interactive)
  (let ((db (read-file-name "Enter db name:")))
    (message db)
    (async-shell-command (concat "sqlite3 " db))
    (other-window 1)
    (rename-buffer (concat "*" db "*"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (geiser counsel ivy slime yasnippet-snippets use-package smartparens modus-vivendi-theme magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
