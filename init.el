; packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives'("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'use-package)

; ido
(setq ido-enable-flex-matching 1
      ido-everwhere t
      ido-use-filename-at-point 'guess
      ido-file-extensions-order '(".org" ".el" ".py" ".pl" ".md" ".markdown"))
(ido-mode 1)

; e-lisp mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c e") 'eval-region)
	    (local-set-key (kbd "C-c E") 'eval-buffer)))
; org-mode

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

(use-package org-bullets
  :ensure t)
(use-package writeroom-mode
  :ensure t)
 (use-package mixed-pitch
  :ensure t)

(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode)
	    (org-bullets-mode)
	    (local-set-key (kbd "C-c o") 'org-open-point)
	    (local-set-key (kbd "C-c e") 'flyspell-auto-correct-word)
	    (flyspell-mode)
	    (variable-pitch-mode)
	    (mixed-pitch-mode)
	    (local-set-key (kbd "C-c C-c") 'writeroom-mode)))

;; fontify org mode source blocks
(setq org-src-fontify-natively t)
;; disable footer stuff in org mode
(setq org-export-html-postamble nil)

(setq org-capture-templates
      '(("e" "email" entry (file+headline "~/stuff.org" "Emails")
         "* %?\n%a\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
        ("p" "process-soon" entry (file+headline "~/stuff.org" "Todo")
	 "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
	("a" "article" entry (file+headline "~/stuff.org" "Articles")
      "* %a \n %i") ))

; lisp mode
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))

; eww
(add-hook 'eww-mode-hook
	  (lambda () (local-set-key (kbd "q") 'kill-this-buffer)))

; Email
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

(add-hook 'mu4e-view-mode-hook (lambda ()
				 (setq truncate-lines t)
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
				 (local-set-key (kbd "i") 'mu4e-inbox)
				 (local-set-key (kbd "I") 'mu4e-update-and-inbox)))

(setq
 mu4e-maildir "~/Maildir"
 mu4e-sent-folder "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-refile-folder "/Archive"
 mu4e-html2text-command 'mu4e-shr2text
 shr-color-visible-luminance-min 60
 shr-color-visible-distance-min 5
 shr-use-colors nil)

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(add-hook 'mu4e-view-mode-hook (lambda ()
				 (local-set-key (kbd "C-c c") 'org-capture)))

; HTML
(add-hook 'html-mode-hook (lambda ()
			    (local-set-key (kbd "C-c f") 'xah-open-file-at-cursor)
			    (rainbow-mode)))

; RSS
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

;;Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

; company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(use-package rainbow-mode
  :ensure t)

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;;Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t))

;; global functions
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2018-02-21"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                                ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (if ; not starting “http://”
          (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
          (let (
                ($fpath (match-string 1 $path))
                ($line-num (string-to-number (match-string 2 $path))))
            (if (file-exists-p $fpath)
                (progn
                  (find-file $fpath)
                  (goto-char 1)
                  (forward-line (1- $line-num)))
              (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                (find-file $fpath))))
        (if (file-exists-p $path)
            (progn ; open f.ts instead of f.js
              (let (($ext (file-name-extension $path))
                    ($fnamecore (file-name-sans-extension $path)))
                (if (and (string-equal $ext "js")
                         (file-exists-p (concat $fnamecore ".ts")))
                    (find-file (concat $fnamecore ".ts"))
                  (find-file $path))))
          (if (file-exists-p (concat $path ".el"))
              (find-file (concat $path ".el"))
            (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
              (find-file $path ))))))))

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

(defun other-window-reverse (&optional x)
  "Cycle back through windows"
  (interactive "P")
  (if (equal x nil)
      (other-window -1)
    (other-window (- 0 x)) ))

(defun jump-to-char (arg char)
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1))

(defun move-word-to-delimiter-backwards ()
  "Moves the word to the nearest delimiter"
  (interactive)
  (kill-word 1)
  (point-to-register ?r)
  (re-search-backward "]\\|\"\\|)}" nil t)
  (yank)
  (jump-to-register ?r))

; colors
(set-face-attribute 'region nil :background "blue")
(set-face-attribute 'region nil :foreground "white")
(set-cursor-color "magenta")

; global modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode)
(show-paren-mode 1)
(global-font-lock-mode 1)
(rainbow-mode)
(global-company-mode 1)
(yas-global-mode)
(delete-selection-mode t)
(global-prettify-symbols-mode 1)

; global key binds
(global-set-key (kbd "C-c f") 'xah-open-file-at-cursor)

(global-set-key (kbd "ESC ESC m") 'mu4e)
(global-set-key (kbd "и") 'mu4e)

(global-set-key (kbd "C-c m") 'menu-bar-mode)


(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-a") 'find-file)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

(global-set-key (kbd "C-z") 'jump-to-char)
(global-set-key (kbd "M-l") 'move-word-to-delimiter-backwards)
(global-set-key (kbd "M-w") 'goto-line)

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

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

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-r") 'redo)

(global-set-key (kbd "C-S-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>")     'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>")      'shrink-window)
(global-set-key (kbd "C-S-<up>")        'enlarge-window)
(global-set-key (kbd "C-x K")         'kill-buffer-and-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-reverse)
(global-set-key (kbd "C-c l") 'linum-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (mixed-pitch mixed-pitch-mode writeroom-mode org-bullets mu4e notmuch yasnippet-snippets use-package rainbow-mode magit expand-region elfeed-org company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit TeX\ Gyre\ Schola\ Regular  :height 250))))
 '(org-level-2 ((t (:inherit TeX\ Gyre\ Schola\ Regular  :height 200))))
 '(org-level-3 ((t (:inherit TeX\ Gyre\ Schola\ Regular  :height 180))))
 '(org-level-4 ((t (:inherit TeX\ Gyre\ Schola\ Regular  :height 160))))
 '(org-level-5 ((t (:inherit TeX\ Gyre\ Schola\ Regular  :height 150))))
 '(variable-pitch ((t (:family "TeX Gyre Schola Regular" :height 145)))))
