;; Packaging
(package-initialize)
(add-to-list 'package-archives'("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives'("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(require 'use-package)

;; Hydra
(use-package hydra
  :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (use-package all-the-icons-ivy
    :ensure t
    :config
    (all-the-icons-ivy-setup)))

;; Evil
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (use-package evil-leader
    :ensure t
    :config
      (evil-leader/set-leader ",")
  (evil-leader/set-key
   "b" 'switch-to-buffer
   "s" 'save-buffer
   "a" 'counsel-find-file)
  (global-evil-leader-mode)))

;; Emacs Lisp
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

(use-package org-bullets
  :ensure t)
 (use-package mixed-pitch
  :ensure t)
(use-package writeroom-mode
  :ensure t
  :config
  (setq  writeroom-mode-line nil))	 

(define-skeleton skeleton-citation
  "Skeleton LaTeX citation"
  nil
  "\\cite{" _ "}")

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

;; Lisp
(add-to-list 'auto-mode-alist '("\\.stumpwmrc\\'" . lisp-mode))
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-hook 'inferior-lisp-mode-hook (lambda ()
				       (inferior-slime-mode t)))

  (setq slime-contribs '(slime-fancy)))

(add-hook 'lisp-mode-hook 'slime-mode)

;; Scheme
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(chez)))

;; CC
(use-package irony
  :ensure t)
;; tabs
(setq-default c-basic-offset 8)
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)

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
     				     (local-set-key (kbd "M-{") 'doc-view-next-page)
				     )))
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

(use-package emmet-mode
  :ensure t)

(add-hook 'html-mode-hook (lambda ()
			    (rainbow-mode)
			    (emmet-mode)
			    (local-set-key (kbd "C-c f") 'xah-open-file-at-cursor)
			    (local-set-key (kbd "C-c i") 'skeleton-list-item)
    			    (local-set-key (kbd "C-c I") 'skeleton-table)
			    (local-set-key (kbd "C-c p") 'skeleton-code)
			    (local-set-key (kbd "C-c P") 'skeleton-pre-code)))
(require 'eww)

(defun eww-current ()
  "Render HTML in the current buffer with EWW"
  (interactive)
  (beginning-of-buffer)
  (eww-display-html 'utf8 (buffer-name)))

;; Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "ц") 'magit-commit)
  (global-set-key (kbd "к") 'magit-stage-modified)
  (global-set-key (kbd "н") 'magit-push))

;; Java
(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
	    (lambda ()
	      (meghanada-mode t))))

;; Minor Modes
;; Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3))

(use-package rainbow-mode
  :ensure t)

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (defhydra hydra-expand-region ()
    "region: "
    ("v" er/expand-region "expand")
    ("x" er/contract-region "contract"))
  (evil-define-key 'visual 'global (kbd "v") 'hydra-expand-region/body))

;; Avy
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-j") 'avy-go-to-char))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t))

;; Auto insert
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.org\\'" . "Org skeleton")
     '(
       "#+SETUPFILE: /home/yur3i/.emacs.d/orgconfig.org" > \n)))

(defun auto-insert-guard ()
   "Prevent auto-insertion for files that exist already"
   (interactive)
   (unless (file-exists-p (buffer-file-name))
     (auto-insert)))

(add-hook 'find-file-hook 'auto-insert-guard)


(add-hook 'dired-mode-hook (lambda ()
			     (dired-hide-details-mode)
			     (local-set-key (kbd "C-c d") 'dired-hide-details-mode)))


;; smart parens
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
(defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)
  
  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)
  
  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)
  
  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)
  
  ;Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))
  (define-prefix-command 'sp-map)
  (global-set-key (kbd "C-t")   'sp-map)
  (global-set-key (kbd "C-t r") 'sp-rewrap-sexp)
  (global-set-key (kbd "C-t {") 'sp-wrap-curly)
  (global-set-key (kbd "C-t [") 'sp-wrap-square)
  (global-set-key (kbd "C-t (") 'sp-wrap-round)
  (global-set-key (kbd "M-n")   'sp-next-sexp)
  (global-set-key (kbd "M-p")   'sp-previous-sexp)
  (evil-leader/set-key
    "p" 'hydra-smartparens/body))


;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package flycheck
  :ensure t)

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
(set-frame-parameter (selected-frame) 'alpha value))

(defvar xah-brackets nil "String of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

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
  (forward-char -1)
  (forward-char))

(defun move-word-to-delimiter-backwards ()
  "Moves the word to the nearest delimiter"
  (interactive)
  (kill-word 1)
  (point-to-register ?r)
  (re-search-backward "]\\|\"\\|)}" nil t)
  (yank)
  (jump-to-register ?r))

(defun xah-toggle-letter-case ()
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

;; colours

(use-package zenburn-theme
	     :ensure t)

(load-theme 'zenburn t)
(set-cursor-color "magenta")
;;(transparency 80)
(set-face-attribute 'region    nil :background "blue")
(set-face-attribute 'region    nil :foreground "white")
;(set-face-attribute 'default   nil :foreground "#FFFFFF")
;(set-face-attribute 'default   nil :background "#000000")
;(set-face-attribute 'fringe    nil :background "#000000")
;;(set-face-attribute 'mode-line nil :box '(:width 0))
;;(set-face-attribute 'mode-line-inactive nil :box '(:width 0))

;; Mode line
(defun d/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))
; global minor modes
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

; global keybinds
(global-set-key (kbd "M-<") 'shrink-window-horizontally)
(global-set-key (kbd "M->") 'grow-window-horizontally)
(global-set-key (kbd "M-,") 'shrink-window)
(global-set-key (kbd "M-,") 'grow-window)

(global-set-key (kbd "M-RET") 'eshell)

(global-set-key (kbd "C-c f") 'xah-open-file-at-cursor)

(global-set-key (kbd "ESC ESC m") 'mu4e)
(global-set-key (kbd "и") 'mu4e)

(global-set-key (kbd "C-c m") 'menu-bar-mode)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") 'save-buffer)
(global-unset-key (kbd "M-a"))
(global-set-key (kbd "M-a") 'find-file)

(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

(global-set-key (kbd "C-z") 'jump-to-char)
(global-set-key (kbd "M-l") 'move-word-to-delimiter-backwards)
(global-set-key (kbd "M-c") 'xah-toggle-letter-case)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
(global-set-key (kbd "M-(") 'xah-backward-left-bracket)
(global-set-key (kbd "M-)") 'xah-forward-right-bracket)
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
(global-set-key (kbd "M-\\") 'shell-command-on-region)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "M-<up>")    'enlarge-window)
(global-set-key (kbd "M-<down>")  'shrink-window)

(global-set-key (kbd "C-c C") 'comment-dwim)

;; disable size hinting
(setq frame-resize-pixelwise t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" default)))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (elfeed hydra evil-leader avy all-the-icons-ivy ivy helm slime-company exwm geiser emmet-mode irony cython-mode cyberpunk-theme multiple-cursors meghanada doom-themes neotree doom-modeline flymake-haskell-multi haskell-mode slime treemacs csharp-mode c-sharp-mode toml-mode rust-mode olivetti mixed-pitch mixed-pitch-mode writeroom-mode org-bullets mu4e notmuch yasnippet-snippets use-package rainbow-mode magit expand-region elfeed-org company)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "SRC" :slant normal :weight normal :height 128 :width normal))))
 '(cursor ((t (:background "magenta" :foreground "black"))))
 '(custom-comment ((t (:background "gray40"))))
 '(font-lock-comment-face ((t (:foreground "Firebrick" :slant italic))))
 '(highlight ((t (:background "#0000FF" :foreground "white"))))
 '(show-paren-match ((t (:background "cyan" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "black")))))
