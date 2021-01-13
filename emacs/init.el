(defvar *pkg-list* '())

(defmacro pkg (pack)
  `(setf *pkg-list* (append *pkg-list* (list ',pack))))

(defun install-deps ()
  (interactive)
  (package-refresh-contents)
  (setf package-selected-packages *pkg-list*)
  (package-install-selected-packages))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "/home/jorde/.emacs.d/modules")
(add-to-list 'load-path "/home/jorde/.emacs.d/lisp")

(require 'module-fun)
(require 'module-misc)
(require 'module-theming)
(require 'module-ido)
(require 'module-keys)
(require 'module-lisp)
(require 'module-email)
(require 'module-music)
;;(require 'module-dired)
(require 'module-py)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "64f57ca127c32c83c79bfb8c48bad7743529ecf6d9bb1a0b922f9a3aa9d962f0" "12fed97801a5adac5711d0fd7c9e0d5c050b37b7c6984429a94f3bbd65ef9443" default))
 '(package-selected-packages
   '(telega sly notmuch magit macrostep hy-mode github-theme geiser expand-region))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 120 :width normal)))))
