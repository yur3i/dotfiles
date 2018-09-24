; packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
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
(defun browse-file-windows (file)
  "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
  (let ((windows-file-name (dired-replace-in-string
                           "/" "\\" (dired-get-filename))))
    (or (condition-case nil
            (w32-shell-execute nil windows-file-name)
          (error nil))
        (find-file windows-file-name))))

(defun browse-file-linux (file)
  (dired-do-shell-command "gnome-open" nil
                          (dired-get-marked-files t current-prefix-arg)))

(defun browse-file (file)
  (cond ((equal system-type 'gnu/linux)
         (browse-file-linux file))
        ((equal system-type 'windows-nt)
(browse-file-windows file))))

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
             (set-buffer-modified-p nil)))))
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

(defun jump-lines-up ()
  (interactive)
  (let* ((key (read-char-choice "Jump lines up? " (number-sequence 49 57)))
         (lines (string-to-number (byte-to-string key))))
    (goto-line (+ (line-number-at-pos) (- lines)))))
(global-set-key (kbd "M-p") 'jump-lines-up)

(defun jump-lines-down ()
  (interactive)
  (let* ((key (read-char-choice "Jump lines down? " (number-sequence 49 57)))
         (lines (string-to-number (byte-to-string key))))
    (goto-line (+ (line-number-at-pos) lines))))
(global-set-key (kbd "M-n") 'jump-lines-down)
  
(defun gtlu (n)
  (interactive "nHow many lines up?: ")
  (goto-line (+ (line-number-at-pos) (* n -1))))
(global-set-key (kbd "M-P") 'gtlu)

(defun gtld (n)
  (interactive "nHow many lines down?: ")
  (goto-line (+ (line-number-at-pos) n)))  
(global-set-key (kbd "M-N") 'gtld)

(defun forward-multiple-words (n)
  "Move forward in a line multiple lines"
  (interactive "nHow many words?"))

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

(defun split-vertical-find-file ()
  "split the frame vertically and find a file"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (find-file))

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
(global-set-key (kbd "Я") (lambda ()
			    (interactive)
			    (shell-command "cp ~/.emacs.d/init.el ~/dotfiles/init.el")
			    (find-file "~/dotfiles/init.el")))
(global-set-key (kbd "ESC ESC C") (lambda ()
			    (interactive)
			    (shell-command "cp ~/.emacs.d/init.el ~/dotfiles/init.el")
			    (find-file "~/dotfiles/init.el")))
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
				    (find-file "~/stuff.org")))
(global-set-key (kbd "м") (lambda()
				    (interactive)
				    (find-file "~/stuff.org")))
(global-set-key (kbd "ESC ESC r") 'elfeed)
(global-set-key (kbd "ESC ESC m") 'mu4e)
(global-set-key (kbd "и") 'mu4e)
(set-cursor-color "magenta")
;;other keybinds for switching buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;;top level keybinds for find-file and switch-to-buffer
(global-set-key (kbd "M-a") 'find-file)
(global-set-key (kbd "M-s") 'switch-to-buffer)
(global-set-key (kbd "M-l") 'save-buffer)
;;disable default keybinds to help get used to M-a and M-s
;;(global-set-key (kbd "C-x C-f") 'keyboard-quit)
;;(global-set-key (kbd "C-x b") 'keyboard-quit)
;;(global-set-key (kbd "C-x C-b") 'keyboard-quit)
;;change C-x k to kill current buffer, more useful than the menu 
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)
(global-set-key (kbd "л") 'kill-buffer)
;;colors and disable window chrome
(set-face-attribute 'region nil :background "blue")
(set-face-attribute 'region nil :foreground "white")
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(global-font-lock-mode 1)
;; kill emacsclient with C-x )
(global-set-key (kbd "C-x )") 'delete-frame)
;;(transparency 80)
;;Second keyboard script
(shell-command "/home/yur3i/.kbd.sh")
;;make stumpwmrc use lisp mode
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))
;;undo
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-r") 'redo)
;;Keybinds for manipulating windows

(global-set-key (kbd "C-S-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>")     'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>")      'shrink-window)
(global-set-key (kbd "C-S-<up>")        'enlarge-window)
(global-set-key (kbd "C-x K")         'kill-buffer-and-window)

(global-set-key (kbd "C-c m") 'menu-bar-mode)
;;E-Shell
(global-set-key (kbd "M-RET") 'eshell)
(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
(global-set-key (kbd "M-S-RET") 'ansi-term-other-window)

;;Hyper Key bindings
(global-set-key (kbd "H-b") 'switch-buffer)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-f") 'find-file)

;;Line numbering
(require 'hlinum)
(hlinum-activate)
(global-set-key (kbd "C-c l") 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-relative-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-relative-mode)
(add-hook 'perl-mode-hook 'linum-mode)
(add-hook 'perl-mode-hook 'linum-relative-mode)
(add-hook 'CC-mode-hook 'linum-mode)
(add-hook 'CC-mode-hook 'linum-relative-mode)
(add-hook 'lisp-mode-hook 'linum-mode)
(add-hook 'lisp-mode-hook 'linum-relative-mode)
(add-hook 'conf-colon-mode-hook 'linum-mode)
(add-hook 'conf-colon-mode-hook 'linum-relative-mode)
(add-hook 'elfeed-search-mode-hook 'linum-mode)
(add-hook 'elfeed-search-mode-hook 'linum-relative-mode)



(setq linum-format "%d")
;;tramp
(setq tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))



;;eww
(add-hook 'eww-mode-hook 
	  (lambda () (local-set-key (kbd "q") 'kill-this-buffer)))


;;Email
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
(global-set-key (kbd "ESC ESC m") 'mu4e)
(global-set-key (kbd "и") 'mu4e)
(add-hook 'mu4e-view-mode-hook (lambda ()
				 (setq truncate-lines t)))
(add-hook 'mu4e-view-mode-hook (lambda ()
				 (visual-line-mode)))
(defun mu4e-inbox ()
  "jump to mu4e inbox"
  (interactive)
  (mu4e~headers-jump-to-maildir "INBOX"))

(defun mu4e-update-and-inbox ()
  "docstring"
  (interactive "P")
  (mu4e-update-index)
  (mu4e-inbox))

(add-hook 'mu4e-main-mode-hook (lambda ()
				 (local-set-key (kbd "i") 'mu4e-inbox)))

(add-hook 'mu4e-main-mode-hook (lambda ()
				 (local-set-key (kbd "I") 'mu4e-update-and-inbox)))

(setq
 mu4e-maildir "~/Maildir"
 mu4e-sent-folder "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-refile-folder "/Archive")

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(add-hook 'mu4e-view-mode-hook (lambda ()
				 (local-set-key (kbd "C-c c") 'org-capture)))


;;Multiple cursors
;((use-package multiple-cursors
;   :ensure t
;   :config
;  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)))

;;RSS
(use-package elfeed
  :ensure t
  :config
  (use-package elfeed-org
    :ensure t)
  (setq elfeed-feeds
	'("http://feeds.bbci.co.uk/news/rss.xml"
	  "https://hnrss.org/frontpage")))
(setq browse-url-browser-function 'eww-browse-url) ; emacs browser for links
(add-hook 'elfeed-search-mode-hook
	  (lambda () (local-set-key (kbd "U") 'elfeed-update)))


;;Org mode
(use-package org-plus-contrib
  :ensure t
  :config
  ;; set default files and directory
  '(org-directory "~/org")
  (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
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
(add-hook 'org-mode-hook 'org-bullets-mode)

;; fontify org mode source blocks
(setq org-src-fontify-natively t)
;; disable footer stuff in org mode
(setq org-export-html-postamble nil)

(defface org-block-begin-line
  '((t (:underline "#111" :foreground "white" :background "#222")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "white" :foreground "black")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#111" :foreground "white" :background "#222")))
  "Face used for the line delimiting the end of source blocks.")
(setq org-capture-templates
      '(("e" "email" entry (file+headline "~/stuff.org" "Emails")
         "* %?\n%a\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
        ("p" "process-soon" entry (file+headline "~/stuff.org" "Todo")
	 "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
	("a" "article" entry (file+headline "~/stuff.org" "Articles")
      "* %a \n %i") ))

(add-to-list 'org-agenda-files "/home/yur3i/stuff.org" 'append)
;; Emacs server

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
(global-set-key (kbd "C-x M-0") 'server-shutdown)
(global-set-key (kbd "C-x )") 'quit-window)

;; Electric pair mode

(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'cc-mode-hook 'electric-pair-mode)
(add-hook 'conf-mode-hook 'electric-pair-mode)
(add-hook 'perl-mode-hook 'electric-pair-mode)
(add-hook 'lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)


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
	  
;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package haskell-mode
  :ensure t)

(use-package engine-mode
  :ensure t
  :config
  (defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :browser 'eww-browse-url))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "95b1450c6a38a211a53fd28c1dcf242b31fcf75d394579e0b11c853423388488" "8bceed439b6d46e0234e0be965cc4d2dc899786d4ce37fbaf10fede43b1cdf79" default)))
 '(cwm-frame-internal-border 70)
 '(display-time-mode t)
 '(markdown-command "/usr/bin/pandoc")
 '(notmuch-print-mechanism (quote notmuch-print-lpr))
 '(notmuch-search-oldest-first nil)
 '(org-babel-load-languages (quote ((C . t) (emacs-lisp . t))))
 '(package-selected-packages
   (quote
    (haskell-mode rust-mode ivy-rich flx mu4e-contrib elfeed-org mu4e notmuch-org notmuch smartparens smart-parens htmlize hlinum zenburn-theme expand-region tabbar emms centered-window centered-window-mode company-jedi company-irony emmet-mode yasnippet-snippets yasnippet markdown-mode sexy-monochrome-theme elfeed-web elfeed multiple-cursors hydra company spaceline doom-themes gruvbox-theme evil paredit smart-mode-line ox-twbs avy rainbow-delimiters swiper-helm counsel ivy rainbow-mode solarized-theme weechat powerline org-bullets telephone-line magit org-plus-contrib exwm)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#444" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 120 :width normal :foundry "xos4" :family "Terminus"))))
 '(company-tooltip ((t (:background "#EEE" :foreground "black"))))
 '(cursor ((t (:background "magenta"))))
 '(fringe ((t (:background "#22240"))))
 '(variable-pitch ((t nil))))
