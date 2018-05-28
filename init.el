;; packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'use-package)

;;set autosave dir
(setq backup-directory-alist '(("." . "~/.autosave")))

;;colors and disable window chrome

(menu-bar-mode -1)
(scroll-bar-mode -1)
;;(load-theme 'tango-dark t)
(fringe-mode 0)
(set-face-attribute 'region nil :background "blue")
(set-face-attribute 'region nil :foreground "white")
(set-background-color "#eee")

;;display time in the mode line

(display-time-mode 1)
(setq display-time-format "%H:%M")

;;Ido

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".el" ".py" ".pl" ".c" ".md" ".markdown"))
(ido-mode 1)

;;functions

(defun org-open-point ()
  "Open org mode heading in another window, expand it, and narrow it"
  (interactive)
  (org-beginning-of-line)
  (setq goal-point (point))
  (call-interactively #'clone-indirect-buffer-other-window)
  (while (not (= goal-point (point)))
    (goto-char goal-point)
    (org-beginning-of-line)
    (org-cycle)
    (goto-char goal-point)
    (org-beginning-of-line))
  (call-interactively #'org-next-visible-heading)
  (narrow-to-region goal-point (point))
  (goto-char goal-point)
  (fset 'tab
	(lambda (&optional arg) "Keyboard macro." (interactive "p")
	  (kmacro-exec-ring-item (quote ([tab] 0 "%d")) arg)))
  (tab))
(global-set-key (kbd "C-c o")          'org-open-point)

(defun kill-whole-line nil
  "kills the entire line on which the cursor is located, and places the 
cursor as close to its previous position as possible."
  (interactive)
  (progn
    (let ((y (current-column))
	  (a (progn (beginning-of-line) (point)))
	  (b (progn (forward-line 1) (point))))
      (kill-region a b)
      (move-to-column y))))
(global-set-key (kbd "C-M-k")          'kill-whole-line)

(defun org-collapsed-point () 
  (interactive)
  (let ((org-M-RET-may-split-line nil) ;don't split header line
        (header-name (read-string "New header: ")))
       (org-meta-return)
       (insert header-name "\n.")
       (previous-line)
       (org-cycle)))
(global-set-key (kbd "C-<return>")     'org-collapsed-point)

(defun org-collapsed-point-and-open () 
  (interactive)
  (let ((org-M-RET-may-split-line nil) ;don't split header line
        (header-name (read-string "New header: ")))
       (org-meta-return)
       (insert header-name "\n.")
       (previous-line)
       (org-cycle)
       (org-open-point)))
(global-set-key (kbd "C-M-<return>")   'org-collapsed-point-and-open)

(defun other-window-reverse (&optional x)
  "Cycle back through windows"
  (interactive "P")
  (if (equal x nil)
      (other-window -1)
    (other-window (- 0 x)) ))
(global-set-key (kbd "C-x p")          'other-window-reverse)

(defun delete-line ()
  "Delete text from current position to end of line char.This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))
(global-set-key (kbd "C-k")           'delete-line)

(defun delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position. This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))
(global-set-key (kbd "C-S-k")         'delete-line-backward)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(global-set-key (kbd "M-d")           'delete-word)

(defun delete-word-backward (arg)
  "Delete characters backward until encountering the beginning of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "<M-backspace>") 'delete-word-backward)

;;Keybinds for manipulating windows

(global-set-key (kbd "C-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-<right>")     'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")      'shrink-window)
(global-set-key (kbd "C-<up>")        'enlarge-window)
(global-set-key (kbd "C-x K")         'kill-buffer-and-window)

;;Multiple cursors
;((use-package multiple-cursors
;   :ensure t
;   :config
;  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)))

;;Org mode
(use-package org-plus-contrib
  :ensure t
  :config
  ;; set default files and directory
  '(org-directory "~/org")
  (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
  '(org-default-notes-file (concat org-directory "/notes.org"))
  ;; structure for emacs lisp code blocks
  (add-to-list 'org-structure-template-alist
	       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")))

;;Company
(use-package company
  :ensure t
  :config
  (global-company-mode 1))
;;Rainbow mode
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'org-mode-hook        'rainbow-mode)
  (add-hook 'css-mode-hook        'rainbow-mode)
  (add-hook 'conf-mode-hook       'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;;Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook       'rainbow-delimiters-mode))
;;Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode))

;;Paredit
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook       'paredit-mode))

;(require 'org)
;(org-babel-load-file
; (expand-file-name "configuration.org"
;                   user-emacs-directory))

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "95b1450c6a38a211a53fd28c1dcf242b31fcf75d394579e0b11c853423388488" "8bceed439b6d46e0234e0be965cc4d2dc899786d4ce37fbaf10fede43b1cdf79" default)))
 '(display-time-mode t)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet markdown-mode sexy-monochrome-theme elfeed-web elfeed multiple-cursors hydra company spaceline doom-themes gruvbox-theme evil paredit smart-mode-line ox-twbs avy rainbow-delimiters swiper-helm counsel ivy rainbow-mode notmuch solarized-theme weechat powerline org-bullets telephone-line magit org-plus-contrib exwm)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "GohuFont" :foundry "Gohu" :slant normal :weight bold :height 105 :width normal)))))
