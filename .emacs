;;my emacs init file


;; package management stuff


(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; load the path ~/.emacs.d/lisp/
(add-to-list 'load-path "~/.emacs.d/lisp/")


;; settings


;; autocomplete brackets etc
(electric-pair-mode 1)
;; set up line numbering and highlight the current line number
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)
;; python settings
(add-hook 'python-mode-hook '(lambda () (linum-on)))
;; E-Lisp settings
(add-hook 'emacs-lisp-mode-hook '(lambda () (linum-on)))
;; CC settings
(add-hook 'c-mode-common-hook '(lambda () (linum-on)))
(defvaralias 'c-basic-offset 'tab-width)
;;set highlight color
(set-face-attribute 'region nil :background "#333")
;set org agenda files
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
;; hide org mode stars
(setq org-hide-leading-stars t)
;; auto complete
(ac-config-default)
;; disable scrollbars
(scroll-bar-mode -1)
;; enable minor modes in org-mode
(add-hook 'org-mode-hook '(lambda () (abbrev-mode) (visual-line-mode) (org-indent)))
;; email
(autoload 'notmuch "notmuch" "notmuch mail" t)
(require 'notmuch)
(setq notmuch-search-oldest-first nil)

;; my functions


(defun night-mode ()
  "Set the background to black"
  (interactive)
  (set-background-color "black")
  (set-foreground-color "white"))

(defun day-mode ()
  "Set the background to white"
  (interactive)
  (set-background-color "white")
  (set-foreground-color "black"))

(defun open-point-org ()
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

(defun compile-latex ()
  "Compiles the current LaTeX document (provided pdflatex is installed)"
  (interactive)
  (shell-command
   (format "pdflatex %s"
	   (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t))



;; keybinds


(global-set-key (kbd "C-c o")    #'open-point-org)
(global-set-key (kbd "C-c c")    #'compile-latex)
(global-set-key (kbd "C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  'shrink-window)
(global-set-key (kbd "C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c a")     'org-agenda)
;;mingus commands
(global-set-key (kbd "M-p P")     'mingus-play-pos)
(global-set-key (kbd "M-p n")     'mingus-next)
(global-set-key (kbd "M-p p")     'mingus-prev)



;; nonsense from package manager


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (notmuch org-bullets mingus auto-complete haskell-mode org-plus-contrib org-edna)))
 '(split-width-threshold 12)
 '(split-window-preferred-function (quote split-window-sensibly)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
