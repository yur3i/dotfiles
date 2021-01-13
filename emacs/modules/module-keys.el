(defvar my-keys-map (make-sparse-keymap)
  "Keymap for `my-keys-mode'.")
(define-minor-mode my-keys-mode
  "Set my keybinds"
  :init-value t
  :lighter " my-keys-mode"
  :keymap my-keys-map)
(define-globalized-minor-mode global-my-keys-mode my-keys-mode my-keys-mode)
(defmacro global-key (key fn)
  `(define-key my-keys-map (kbd ,key) ,fn))

(defmacro file-binding (key loc)
  `(global-key ,(concat "C-c C-g " key) (lambda ()
					  (interactive)
					  (find-file ,loc))))


;; Global
(define-key prog-mode-map (kbd"<tab>") 'indent-or-hippie)
(add-hook 'minibuffer-setup-hook (lambda ()
				   (global-my-keys-mode -1)))
(add-hook 'minibuffer-exit-hook (lambda () (global-my-keys-mode 1)))

(global-key "M-o" 'other-window)
(global-key "M-u" 'undo)
(global-key "M-]" 'next-buffer)
(global-key "M-[" 'previous-buffer)
(global-key "M-z" 'my-repeat)
(global-key "M-c" 'toggle-letter-case)
(global-key "M-P" 'scroll-backward-fast)
(global-key "M-N" 'scroll-forward-fast)


(global-key "C-z" 'jump-to-char)
(global-key "C-w" 'cut-line-or-region)
(global-key "C-=" 'er/expand-region)

(global-key "C-M-9" 'surround-sexp)
(global-key "C-M-k" 'kill-whole-line)


(global-key "C-x k" 'kill-this-buffer)
(global-key "C-x K" 'kill-buffer-and-window)
(global-key "C-x F" 'find-file-at-point)
(global-key "C-x 2" (lambda () 
		      (interactive)
		      (split-window-below)
		      (other-window 1)))
(global-key "C-x 3" (lambda () 
		      (interactive)
		      (split-window-right)
		      (other-window 1)))

(global-key "C-c i" 'ido-imenu)
(global-key "C-c f" 'move-point-forward-out-of-string)
(global-key "C-c b" 'move-point-backward-out-of-string)
(global-key "C-c a" 'arrayify)
(global-key "C-c m" 'bongo)

(global-key "s-c" 'clear-dwim)
(global-key "s-e" 'clear-to-end)

(file-binding "m" "~/mus")
(file-binding "f" "~/proj/fyp")

(provide 'module-keys)
