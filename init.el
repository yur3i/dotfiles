
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Mono" :foundry "CTDB" :slant normal :weight normal :height 143 :width normal))))
 '(custom-safe-themes (quote ("873d8b58357aecbeedd0acdda2aca3f3f5b92ceb4a5dbe9384a4837fe1e34aa3" default)))
 '(package-selected-packages (quote (all-the-icons neotree powerline gotham-theme))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1118c0b29f79145d4a144011cd8b6c804791b2d6dd7e37e9cc45b3fbfdf8d7f9" "50b66fad333100cc645a27ada899a7b1d44f1ceb32140ab8e88fedabfb7d0daf" "61ae193bf16ef5c18198fbb4516f0c61a88f7b55b693a3b32d261d8501c4a54b" "473c69b2e448e37861e2051f793a8981ac419cc06ac66b2be6c08fddcf898175" "0ca5a450034c92069769e071e63a3d2b2346c304bf186245467f59d993f5b979" "873d8b58357aecbeedd0acdda2aca3f3f5b92ceb4a5dbe9384a4837fe1e34aa3" "fdc151aa2dd9f5b2cf118ad35e62e99c04191894a4c37e39a236deaf18fb035f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (dashboard solarized-theme org-plus-contrib org-bullets kaolin-themes gotham-theme faff-theme anti-zenburn-theme all-the-icons)))
 '(split-height-threshold nil)
 '(split-width-threshold 20))
 '(package-selected-packages
   (quote
    (org-plus-contrib org-bullets anti-zenburn-theme kaolin-themes faff-theme solarized-theme gotham-theme all-the-icons)))


 
