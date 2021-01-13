(pkg modus-themes)
(pkg prism)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'modus-vivendi t)
(set-face-attribute 'region nil :background "blue")
(set-face-attribute 'region nil :foreground "white")

(setq prism-comments nil)
(setq prism-num-faces 8)
(prism-set-colors
  :desaturations '(0) ; may lower the contrast ratio
  :lightens '(0)      ; same
  :colors (modus-themes-with-colors
            (list fg-special-cold
                  magenta
                  magenta-alt-other
                  cyan-alt-other
                  fg-main
                  blue-alt
                  red-alt-other
                  cyan)))
(add-hook 'prog-mode-hook 'prism-mode)
(add-hook 'python-mode-hook 'prism-whitespace-mode)



(provide 'module-theming)
