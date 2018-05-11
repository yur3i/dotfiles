
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "95b1450c6a38a211a53fd28c1dcf242b31fcf75d394579e0b11c853423388488" "8bceed439b6d46e0234e0be965cc4d2dc899786d4ce37fbaf10fede43b1cdf79" default)))
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (spaceline doom-themes gruvbox-theme evil paredit smart-mode-line ox-twbs avy rainbow-delimiters swiper-helm counsel ivy rainbow-mode notmuch solarized-theme weechat powerline org-bullets telephone-line magit org-plus-contrib exwm)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
