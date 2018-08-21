;; packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'use-package)
(require 'bind-key)

;;set autosave dir
(setq backup-directory-alist '(("." . "~/.autosave")))

;;tabs
(setq-default c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)

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
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
t))))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

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
  (tab)
  (visual-line-mode))
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

(defun jump-to-char (arg char)
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1))
(global-set-key (kbd "C-z") 'jump-to-char)

(defun delete-line ()
  "Delete text from current position to end of line char.This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))
(global-set-key (kbd "<C-k>")           'delete-line)

(defun delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position. This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))
(global-set-key (kbd "<C-S-k>")         'delete-line-backward)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(bind-key "M-d" 'delete-word)

(defun delete-word-backward (arg)
  "Delete characters backward until encountering the beginning of a word. With argument, do this that many times. This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word (- arg)))


(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda() (local-set-key (kbd "C-c e") 'eval-region)))


;; shortcuts
(global-set-key (kbd "<M-backspace>") 'delete-word-backward)
(global-set-key (kbd "M-DEL") 'delete-word)

;;; File Shortcuts
(global-set-key (kbd "я") (lambda()
				    (interactive)
				    (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "Я") (lambda ())
		(shell-command "cp /home/yur3i/.emacs.d/init.el /home/yur3i/dotfiles/init.el")
		(find-file "~/dotfiles/init.el"))
(global-set-key (kbd "ESC ESC c") (lambda()
				    (interactive)
				    (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "ESC ESC d") (lambda()
				    (interactive)
				    (find-file "~/Documents")))
(global-set-key (kbd "ч") (lambda()
				    (interactive)
				    (find-file "~/Documents")))
(global-set-key (kbd "ESC ESC p") (lambda()
				    (interactive)
				    (find-file "~/Personal.org")))
(global-set-key (kbd "м") (lambda()
				    (interactive)
				    (find-file "~/Personal.org")))
(global-set-key (kbd "ESC ESC m") 'notmuch-hello)
(global-set-key (kbd "и") 'notmuch-hello)
;;other keybinds for switching buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;;top level keybinds for find-file and switch-to-buffer
(global-set-key (kbd "M-a") 'find-file)
(global-set-key (kbd "M-s") 'switch-to-buffer)

;;change C-x k to kill current buffer, more useful than the menu
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)
(global-set-key (kbd "л") 'kill-buffer)
;;colors and disable window chrome
(set-face-attribute 'region nil :background "blue")
(set-face-attribute 'region nil :foreground "white")
;;(set-background-color "black")
;;(set-foreground-color "white")
(fringe-mode 0)
(set-face-attribute 'fringe nil :background nil)
;(load-theme 'leuven t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; kill emacsclient with C-x )
(global-set-key (kbd "C-x )") 'delete-frame)
;;(transparency 80)
;;Second keyboard script
(shell-command "/home/yur3i/.kbd.sh")

;;Keybinds for manipulating windows

(global-set-key (kbd "C-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-<right>")     'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")      'shrink-window)
(global-set-key (kbd "C-<up>")        'enlarge-window)
(global-set-key (kbd "C-x K")         'kill-buffer-and-window)

;;E-Shell
(global-set-key (kbd "M-RET") 'eshell)
(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
(global-set-key (kbd "H-RET") 'eshell-other-window)

;;Hyper Key bindings
(global-set-key (kbd "H-b") 'switch-buffer)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-f") 'find-file)

;;Line number keybinding
(global-set-key (kbd "C-c l") 'linum-mode)
;;tramp
(setq tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
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
	       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-hook 'org-mode-hook
	  (lambda() (local-set-key (kbd "C-c e") 'flyspell-auto-correct-word)))
(add-hook 'org-mode-hook
	  (lambda() (local-set-key (kbd "т") 'org-export-dispatch)))
;; fontify org mode source blocks
(setq org-src-fontify-natively t)
;; disable footer stuff in org mode
(setq org-export-html-postamble nil)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#777")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

;; Emacs server

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(global-set-key (kbd "C-x )") 'quit-window)

;; Electric pair mode

(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'cc-mode-hook 'electric-pair-mode)
(add-hook 'conf-mode-hook 'electric-pair-mode)
(add-hook 'perl-mode-hook 'electric-pair-mode)

;;Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode 1))

;;C family
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cbd-autosetup-compile-options))



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

;;Magit
(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)
    (bind-key "й" 'magit-status)
    (bind-key "ц" 'magit-commit)
    (bind-key "к" 'magit-stage-modified)
    (bind-key "н" 'magit-push)))

;;Emmet Mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;;Flymake
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'lisp-mode-hook       'flymake-mode)
(add-hook 'c-mode-hook          'flymake-mode)
(add-hook 'python-mode-hook     'flymake-mode)
(add-hook 'org-mode-hook        'flyspell-mode)

;quickly overwrite highlight
(delete-selection-mode t)

;; Centered Text mode 
(require 'centered-window)
(global-set-key (kbd "C-c C-c") 'centered-window-mode)
;; EXWM
(use-package exwm
  :ensure t
  :config
;  (require 'exwm)
;  (require 'exwm-config)
;  (exwm-config-default)
  )
;;open an org mode config file
;(require 'org)
;(org-babel-load-file
; (expand-file-name "configuration.org"
;                   user-emacs-directory))

;;EMMS
(use-package emms
  :ensure t
  :config
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all) 
    (setq emms-seek-seconds 5)
    (setq emms-player-list '(emms-player-mpd))
    (setq emms-info-functions '(emms-info-mpd))
    (setq emms-player-mpd-server-name "localhost")
    (setq emms-player-mpd-server-port "6600")
  :bind
    ("s-m p" . emms)
    ("s-m b" . emms-smart-browse)
    ("s-m r" . emms-player-mpd-update-all-reset-cache)
    ("<XF86AudioPrev>" . emms-previous)
    ("<XF86AudioNext>" . emms-next)
    ("<XF86AudioPlay>" . emms-pause)
    ("<XF86AudioStop>" . emms-stop))

;;Tabbar
(use-package tabbar
  :ensure t
  :config
  (when (require 'tabbar nil t)
    (setq tabbar-buffer-groups-function
          (lambda () (list "All Buffers")))
    (setq tabbar-buffer-list-function
          (lambda ()
            (remove-if
             (lambda(buffer)
               (find (aref (buffer-name buffer) 0) " *"))
             (buffer-list))))
    (tabbar-mode)))

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "95b1450c6a38a211a53fd28c1dcf242b31fcf75d394579e0b11c853423388488" "8bceed439b6d46e0234e0be965cc4d2dc899786d4ce37fbaf10fede43b1cdf79" default)))
 '(cwm-frame-internal-border 70)
 '(display-time-mode t)
 '(markdown-command "/usr/bin/pandoc")
 '(org-babel-load-languages (quote ((C . t) (emacs-lisp . t))))
 '(package-selected-packages
   (quote
    (expand-region tabbar emms centered-window centered-window-mode company-jedi company-irony emmet-mode yasnippet-snippets yasnippet markdown-mode sexy-monochrome-theme elfeed-web elfeed multiple-cursors hydra company spaceline doom-themes gruvbox-theme evil paredit smart-mode-line ox-twbs avy rainbow-delimiters swiper-helm counsel ivy rainbow-mode notmuch solarized-theme weechat powerline org-bullets telephone-line magit org-plus-contrib exwm)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "unknown" :slant normal :weight normal :height 119 :width normal)))))

