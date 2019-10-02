;; Packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives'("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(require 'use-package)

;; (load "~/.emacs.d/init/install-packages.el")

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".java" ".py" ".sh" ".el" ".org" ".md" ".txt")
      )
(ido-mode 1)

(set-face-attribute 'default nil :background "#111")
(set-face-attribute 'default nil :foreground "white")
(set-face-attribute 'mode-line nil :box nil)
(set-cursor-color "red")
(set-face-attribute 'region nil :background "blue")
(set-fringe-mode '(5 . 0))

(define-skeleton skeleton-lsk
  "skeleton code for a local-set-key"
  nil
  "(local-set-key (kbd \"" _ "\") ')")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e") 'eval-region)
            (local-set-key (kbd "C-c E") 'eval-buffer)
            (local-set-key (kbd "C-c k") 'skeleton-lsk)))
;; Org Mode
(defun org-tree-open-in-right-frame ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(define-skeleton skeleton-citation
  "Skeleton LaTeX citation"
  nil
  "\\cite{" _ "}")

(require 'ox-beamer)
(unless (assoc "beamer" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode)
            (local-set-key (kbd "C-c o") 'org-tree-to-indirect-buffer)
            (local-set-key (kbd "C-c O") (lambda ()
                                           (org-tree-to-indirect-buffer)
                                           (other-window)
                                           (next-line)))
            (local-set-key (kbd "C-c E") 'my-save-word)
            (local-set-key (kbd "C-c C-c") 'writeroom-mode)
            (local-set-key (kbd "C-c c")   'count-words-region)
            (local-set-key (kbd "C-c M-c") 'count-words)
            (local-set-key (kbd "C-c r") 'skeleton-citation)
            (local-set-key (kbd "C-c s") 'org-schedule)
            (local-set-key (kbd "C-c e") 'org-ctrl-c-ctrl-c)
            (local-unset-key (kbd "M-a"))))
(setq  writeroom-mode-line nil)
;; Lisp
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))
(setq inferior-lisp-program "/usr/bin/clisp"
      slime-contribs '(slime-fancy))
(add-hook 'inferior-lisp-mode-hook (lambda ()
                                     (inferior-slime-mode t)))
(add-hook 'lisp-mode-hook (lambda ()
                            (slime-mode)
                            (local-set-key (kbd "C-c e") 'slime-eval-last-expression-in-repl)
                            (local-set-key (kbd "C-c r") 'slime-eval-region)))
;; Scheme
(add-hook 'scheme-mode-hook (lambda ()
                              (geiser-mode)
                              (local-set-key (kbd "C-c r") 'geiser-eval-region)
                              (local-set-key (kbd "C-c e") 'geiser-eval-last-sexp)
                              (local-set-key (kbd "C-c B") 'geiser-eval-buffer)))
(add-hook 'geiser-repl-mode-hook (lambda ()
								   (company-mode nil)))


(setq geiser-active-implementations '(chez guile))
(setq geiser-default-implementation 'guile)
;; RSS
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"
        "http://feeds.reuters.com/reuters/UKdomesticNews"
        "http://feeds.reuters.com/reuters/technologyNews"
        "https://edavis.github.io/hnrss/#firehose-feeds"))
;; Tabs
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
;; Doc View
(defun doc-view-rotate-current-page ()
  "Rotate the current page by 90 degrees.  Requires ImageMagick installation"
  (interactive)
  (when (eq major-mode 'doc-view-mode)
    ;; we are assuming current doc-view internals about cache-names
    (let ((file-name (expand-file-name (format "page-%d.png" (doc-view-current-page)) (doc-view--current-cache-dir))))
      ;; assume imagemagick is installed and rotate file in-place and redisplay buffer
      (call-process-shell-command "convert" nil nil nil "-rotate" "90" (concat "\"" file-name "\"") (concat "\"" file-name "\""))
      (clear-image-cache)
      (doc-view-goto-page (doc-view-current-page)))))
(add-hook 'dov-view-mode-hook (lambda () (
                                          (auto-revert-mode)
                                          (local-set-key (kbd "M-{") 'doc-view-previous-page)
                                          (local-set-key (kbd "M-{") 'doc-view-next-page))))
;; HTML
(define-skeleton skeleton-list-item
  "HTML list item tags"
  nil
  "<li>" _ "</li>")

(define-skeleton skeleton-table
  "HTML table with a list"
  nil
  "<ul>\n\t<li>" _ "</li>\n</ul>")

(define-skeleton skeleton-code
  "HTML code tags"
  nil
  "<code>" _ "</code>")

(define-skeleton skeleton-pre-code
  "Create a nested <code> tag in a <pre> tag"
  nil
  "<pre><code>\n" _ "\n</code></pre>")
(add-hook 'html-mode-hook (lambda ()
                            (rainbow-mode)
                            (emmet-mode)
                            (local-set-key (kbd "C-c f") 'xah-open-file-at-cursor)
                            (local-set-key (kbd "C-c i") 'skeleton-list-item)
                            (local-set-key (kbd "C-c I") 'skeleton-table)
                            (local-set-key (kbd "C-c p") 'skeleton-code)
                            (local-set-key (kbd "C-c P") 'skeleton-pre-code)))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
(add-hook 'web-mode-hook (lambda ()
                           (rainbow-mode)
                           (emmet-mode)
                           (local-set-key (kbd "C-c f") 'xah-open-file-at-cursor)
                           (local-set-key (kbd "C-c i") 'skeleton-list-item)
                           (local-set-key (kbd "C-c I") 'skeleton-table)
                           (local-set-key (kbd "C-c p") 'skeleton-code)
                           (local-set-key (kbd "C-c P") 'skeleton-pre-code)))
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "ц") 'magit-commit)
(global-set-key (kbd "к") 'magit-stage-modified)
(global-set-key (kbd "н") 'magit-push)
;; Projectile
(projectile-mode 1)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c C-p") 'projectile-command-map)
(setq projectile-project-search-path '("~/projects/")
      projectile-completion-system 'ido)
;; Java
(defun fieldgen (type field)
  "Inserts a Java field, and getter/setter methods."
  (interactive "MType: \nMField: ")

  (let ((oldpoint (point))
        (capfield (concat (capitalize (substring field 0 1)) (substring field 1)))
        )
    (insert (concat "public " type " get" capfield "()\n"
                    "{\n"
                    "    return this." field ";\n"
                    "}\n\n"
                    "public void set" capfield "(" type " " field ")\n"
                    "{\n"
                    "    this." field " = " field ";\n"
                    "}\n"))
    (c-indent-region oldpoint (point) t)))
;; Company
(setq company-idle-delay 0
      company-minimum-prefix-length 3)
;; Go
(setq gofmt-command "/home/jorde/go/bin/goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
;; LSP
(add-hook 'prog-mode-hook 'lsp)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-peek-enable t)
;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(defhydra hydra-expand-region ()
  "region: "
  ("v" er/expand-region "expand")
  ("V" er/contract-region "contract"))
;;Paredit
(add-hook 'prog-mode-hook 'paredit-mode)


;; Smartparens
;; (require 'smartparens-config)
;; (smartparens-global-mode)
;; (defhydra hydra-smartparens (:hint nil)
;;   "
;;  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
;; ------------------------------------------------------------------------------------------------------------------------
;;  [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
;;  [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
;;  [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
;;  [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
;;   ;; Moving
;;   ("a" sp-beginning-of-sexp)
;;   ("e" sp-end-of-sexp)
;;   ("f" sp-forward-sexp)
;;   ("b" sp-backward-sexp)
;;   ("n" sp-down-sexp)
;;   ("N" sp-backward-down-sexp)
;;   ("p" sp-up-sexp)
;;   ("P" sp-backward-up-sexp)

;;   ;; Slurping & barfing
;;   ("h" sp-backward-slurp-sexp)
;;   ("H" sp-backward-barf-sexp)
;;   ("l" sp-forward-slurp-sexp)
;;   ("L" sp-forward-barf-sexp)

;;   ;; Wrapping
;;   ("R" sp-rewrap-sexp)
;;   ("u" sp-unwrap-sexp)
;;   ("U" sp-backward-unwrap-sexp)
;;   ("(" sp-wrap-round)
;;   ("{" sp-wrap-curly)
;;   ("[" sp-wrap-square)

;;   ;; Sexp juggling
;;   ("S" sp-split-sexp)
;;   ("s" sp-splice-sexp)
;;   ("r" sp-raise-sexp)
;;   ("j" sp-join-sexp)
;;   ("t" sp-transpose-sexp)
;;   ("A" sp-absorb-sexp)
;;   ("E" sp-emit-sexp)
;;   ("o" sp-convolute-sexp)

;;                  ;Destructive editing
;;   ("c" sp-change-inner :exit t)
;;   ("C" sp-change-enclosing :exit t)
;;   ("k" sp-kill-sexp)
;;   ("K" sp-backward-kill-sexp)
;;   ("w" sp-copy-sexp)

;;   ("q" nil)
;;   ("g" nil))
(define-prefix-command 'sp-map)
(global-set-key (kbd "C-t")   'sp-map)
(global-set-key (kbd "C-t r") 'sp-rewrap-sexp)
(global-set-key (kbd "C-t {") 'sp-wrap-curly)
(global-set-key (kbd "C-t [") 'sp-wrap-square)
(global-set-key (kbd "C-t (") 'sp-wrap-round)
(global-set-key (kbd "M-n")   'sp-next-sexp)
(global-set-key (kbd "M-p")   'sp-previous-sexp)

;; Telegram
(use-package telega
  :load-path "~/src/telega.el"
  :commands (telega)
  :defer t
  :config
  (setq telega-use-notifications t)
  (add-hook 'telega-chat-mode-hook (lambda ()
                                     (local-set-key (kbd "C-c r") 'telega-msg-reply)
                                     (local-set-key (kbd "C-c p") 'telega-chatbuf-attach-photo)
                                     (local-set-key (kbd "C-c f") 'telega-chatbuf-attach)
                                     (local-set-key (kbd "C-c e") 'telega-msg-edit))))

;; Highlights TODO, FIXME etc
(add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords
   nil'(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face
   t)))))
;; Frog jump buffer
(setq frog-jump-buffer-ignore-buffers '("\*.*\*" "Telega*"))
;; Dired

;; Functions
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(defalias 'iwb 'indent-whole-buffer)

(defun ansi-term-toggle ()
  "Toggle ansi-term window on and off with the same command."
  (interactive)
  (defvar my--ansi-term-name "ansi-term-popup")
  (defvar my--window-name (concat "*" my--ansi-term-name "*"))
  (cond ((get-buffer-window my--window-name)
         (ignore-errors (delete-window
                         (get-buffer-window my--window-name))))
        (t (split-window-below)
           (other-window 1)
           (cond ((get-buffer my--window-name)
                  (switch-to-buffer my--window-name))
                 (t (ansi-term "bash" my--ansi-term-name))))))

(global-set-key (kbd "C-x C-t") 'ansi-term-toggle)
(global-set-key (kbd "C-x t")   'ansi-term-toggle)

(defun file-at-point ()
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
(global-set-key (kbd "M-k") 'kill-sexp)

(defun other-window-reverse (&optional x)
  "Cycle back through windows"
  (interactive "P")
  (if (equal x nil)
      (other-window -1)
    (other-window (- 0 x)) ))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-S-o") 'other-window-reverse)

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
;; Minor Modes
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
(global-flycheck-mode)
;; keys
(global-set-key (kbd "æ") 'frog-jump-buffer)
(global-set-key (kbd "M-RET") 'eshell)

(global-set-key (kbd "C-c m") 'menu-bar-mode)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-a") 'find-file)

(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x K")   'kill-buffer-and-window)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

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
                                    (find-file "~/life.org")))
(global-set-key (kbd "м") (lambda()
                            (interactive)
                            (find-file "~/life.org")))

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-r") 'redo)

(define-prefix-command 'leader-key)
(global-set-key (kbd "S-SPC") 'leader-key)
(global-set-key (kbd "S-SPC a") 'find-file)
(global-set-key (kbd "S-SPC w") 'yank)
(global-set-key (kbd "S-SPC p") 'hydra-smartparens/body)
(global-set-key (kbd "S-SPC v") 'hydra-expand-region/body)
(global-set-key (kbd "S-SPC f") 'projectile-find-file)
(global-set-key (kbd "S-SPC b") 'projectile-switch-to-buffer)
(global-set-key (kbd "S-SPC c") 'projectile-switch-project)

(global-set-key (kbd "M-W") 'popup-kill-ring)
(global-set-key (kbd "M-S-SPC") 'rectangle-mark-mode)
(setq frame-resize-pixelwise t)
(fset 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
	(haskell-mode diredplus dired+ frog-jump-buffer smart-mode-line pdf-tools paredit parinfer esup popup-kill-ring go-mode use-package)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal))))
 '(font-lock-function-name-face ((t (:foreground "SpringGreen1" :slant italic)))))
(put 'narrow-to-region 'disabled nil)
