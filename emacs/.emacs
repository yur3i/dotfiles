(server-start)

;; packaging
(require 'package)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
;; Theming
(setq default-frame-alist '((font . "-*-terminus-medium-*-*-*-14-140-*-72-c-*-*-*")
			    (vertical-scroll-bars . nil)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (set-background-color "#fafafa")
;; (set-foreground-color "#000")
(set-cursor-color "red")
(set-face-attribute 'mode-line nil :background "#e3ffff")
;; Selectrum/Ido
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1)
  (use-package selectrum-prescient
    :ensure t)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
;; Paredit
(use-package paredit
  :ensure t)
;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t))
;; git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
;; org mode
(use-package org
  :ensure org-plus-contrib
  :config
  (require 'org-tempo)
  (require 'org)
  (require 'ox-latex)
  (setq org-src-fontify-natively t)
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (add-to-list 'org-latex-packages-alist '("" "color" nil))
  (add-to-list 'org-latex-packages-alist '("" "xcolor" nil))
  (add-to-list 'org-latex-packages-alist '("" "courier" nil))    
  ;; (add-to-list 'org-latex-packages-alist '("" "times" nil))  
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(
				     ("backgroundcolor" "\\color{lightgray!20}")
				     ("keywordstyle" "\\color{magenta}")
				     ("commentstyle" "\\color{green}")
				     ("stringstyle" "\\color{orange}")
				     ("basicstyle" "\\ttfamily\\footnotesize")
				     ("numbers" "left")
				     ("showtabs" "false")
				     ("showspaces" "false")
				     ("showstringspaces" "false")
				     ("breaklines" "true")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t)))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (defun org-table-with-shrunk-field (arg)
    0))
;;php-mode
(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'lsp))
;;web-mode
(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'lsp)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))
;; prog mode specific stuff
(bind-keys :map prog-mode-map
	   ("M-k" . kill-sexp)
	   ("C-j" . company-complete))
(add-hook 'prog-mode-hook 'yas-minor-mode)
;; global keys
(bind-keys*
 ("M-]" . next-buffer)
 ("M-[" . previous-buffer)
 ("M-o" . other-window)
 ("M-u" . undo)
 ("M-s" . sp-splice-sexp)
 ("C-x k" . kill-this-buffer)
 ("C-x K" . kill-buffer-and-window) 
 ("C-x 2" . (lambda () (interactive)(split-window-vertically) (other-window 1)))
 ("C-x 3" . (lambda () (interactive)(split-window-horizontally) (other-window 1))))

;; global modes
(delete-selection-mode 1)
(show-paren-mode 1)
(paredit-mode)

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

(defun repeat-complex-command-no-confirm ()
  "Repeat the lasts complex command without confirming"
  (interactive)
  (let* ((hist  command-history)
         newcmd)
    (while (eq 'repeat-complex-command-no-confirm (caar hist))
      (setq hist  (cdr hist)))
    (setq newcmd  (car hist))
    (if newcmd
        (apply #'funcall-interactively (car newcmd)
               (mapcar (lambda (ee) (eval ee t)) (cdr newcmd)))
      (error "There are no previous complex commands to repeat"))))

(defun my-repeat ()
  "Repeats the last command, complex or otherwise without confirming"
  (interactive)
  (if (eq last-command (caar command-history))
      (repeat-complex-command-no-confirm)
    (call-interactively last-command))
  (setq this-command  last-command))

(global-set-key (kbd "M-q") 'my-repeat)

(defun my-org-open-as-pdf ()
  (interactive)
  (call-process-shell-command (concat "mupdf-x11 " (replace-regexp-in-string "\.org$" ".pdf" (buffer-name)))))
(bind-keys :map org-mode-map
	   ("C-c o" . my-org-open-as-pdf))

(defun roll (n)
  "Roll a dice"
  (interactive "nRoll: ")
  (insert (number-to-string (random n))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-structure-template-alist
   (quote
    (("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "equation")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(package-selected-packages
   (quote
    (selectrum-prescient selectrum leuven-theme paredit yasnippet transmission php-mode company-lsp lsp-company company company-mode lsp-ivy lsp-mode flycheck web-mode counsel-tramp vuiet org-plus-contrib org-contrib geiser counsel ivy slime yasnippet-snippets use-package smartparens modus-vivendi-theme magit)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#e3ffff" :foreground "black" :box (:line-width 1 :color "black" :style released-button))))))
