(in-package :stumpwm)
(set-prefix-key (kbd "C-i"))
(defvar *progs* (list))

(defun load-file (file)
  (load (concatenate 'string (stumpwm::getenv "HOME") "/.stumpwm.d/" file ".lisp")))

(load-file "shifting")
(load-file "defprog")
(load-file "defmenu")

(defprog emacs "e" "emacs" '(:class "Emacs"))
(defprog firefox "f" "firefox" '(:class "Firefox"))
(defprog libreoffice "o" "libreoffice" '(:class "libreoffice"))



(defun get-filenames-list (list retlist)
  (if list
      (get-filenames-list (cdr list) (append retlist (list (list (subseq (namestring (car list)) 18 (length (namestring (car list))))
								 (namestring (car list))))))
    retlist))

(defmenu music "m"
  (get-filenames-list (union (uiop:directory-files (concatenate 'string (stumpwm::getenv "HOME") "/Music/")) 
			     (uiop:subdirectories (concatenate 'string (stumpwm::getenv "HOME") "/Music/")))
		      '())
  (run-shell-command (concatenate 'string (concatenate
					   'string "mpv --no-video "
					   choice) " --input-ipc-server=/tmp/mpvsocket")))

(defcommand play-pause-music () ()
  (run-shell-command "echo'{\"command\": [\"cycle\", \"pause\"]}' | socat - /tmp/mpvsocket"))

(defparameter *programs*
  '(("Telegram" "emacsclient -c -e '(telega nil)'")
    ("Email" "emacsclient -c -e '(notmuch)'")
    ("GIMP" "gimp")
    ("Web" "firefox")
    ("NeXt" "export $(dbus-launch); next")))

(defcommand app-menu () ()
  (labels ((pick (options)
             (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
               (cond
                 ((null selection)
                  (throw 'stumpwm::error "Abort."))
                 ((stringp (second selection))
                  (second selection))
                 (t
                  (pick (cdr selection)))))))
    (let ((choice (pick *programs*)))
      (run-shell-command choice))))

(defcommand vsplit-and-switch () ()
	"Splits vertically and switches to next window"
	(vsplit)
	(fnext))

(defcommand hsplit-and-switch () ()
	"Splits horizontally and switches to next window"
	(hsplit)
	(fnext))

;; keybinds
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-]") "next-in-frame")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-[") "prev-in-frame")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-n") "fnext")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-p") "fother")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-F") "fullscreen")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-RET") "exec urxvt")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-d") "app-menu")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-q") "delete")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-s") "vsplit-and-switch")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-S") "hsplit-and-switch")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-r") "remove")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-R") "kill-and-remove")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-w") "iresize")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-l") "windowlist")

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-0") "select-window-by-number 0")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-1") "select-window-by-number 1")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-2") "select-window-by-number 2")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-3") "select-window-by-number 3")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-4") "select-window-by-number 4")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-5") "select-window-by-number 5")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-6") "select-window-by-number 6")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-7") "select-window-by-number 7")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-8") "select-window-by-number 8")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-9") "select-window-by-number 9")

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-)") "pull 0")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-!") "pull 1")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-\"") "pull 2")
;(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-\£") "pull 3")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-$") "pull 4")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-%") "pull 5")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-^") "pull 6")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-&") "pull 7")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-*") "pull 8")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-(") "pull 9")

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "s")  "exec xfce4-screenshooter")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "f")  "float-this")

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-m") "music-menu")
(defvar *my-music-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "p") "play-pause-music")
    m ; NOTE: this is important
  ))

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "m") '*my-music-bindings*)




;; settings

(setf *mouse-focus-policy*  :sloppy) ;; :click :ignore :sloppy -- Focus follows mouse
(setf *window-border-style* :thin) ;; :none :thick :thin :tight -- no borders
(set-win-bg-color "#FAFAFA")
(set-focus-color "red")
(set-unfocus-color "#FAFAFA")

;; bar
;; (set-module-dir "/home/jorde/.stumpwm.d/modules/")
;; (load-module "cpu")
;; (load-module "disk")
;; (load-module "mem")
;; (load-module "battery-portable")
;; (load-module "maildir")

;; (setf *mode-line-border-width* 0)
;; (setf *screen-mode-line-format*
;; 	  (list "%W"
;; 			'(:eval (stumpwm:run-shell-command "echo" t))
;; 			"%C | %M | BAT: %B | "
;; 			'(:eval (stumpwm:run-shell-command "date '+%a %b %d %H:%M'" t))))
;; (setf *mode-line-background-color* "#111111")
;; (setf *mode-line-foreground-color* "#CCC")
;; (setf *message-window-gravity* :center)
;; (setf *input-window-gravity* :center)
;; (mode-line)
;; gaps
;; (load-module "swm-gaps")
;; (setf swm-gaps:*inner-gaps-size* 5)
;; (setf swm-gaps:*outer-gaps-size* 5)

;; wallpaper
(run-shell-command "feh --bg-fill /home/jorde/Pictures/wallpapers/ansi.png")

;; Audio
(setf *key-codes*
      '((162 . "XF86AudioPlay")		   ; handled by amarok (or other mp3 players)
	(164 . "XF86AudioStop")		   
	(144 . "XF86AudioPrev")
	(153 . "XF86AudioNext")
	(160 . "XF86AudioMute")
	(174 . "XF86AudioLowerVolume")	   ; we use amixer (alsa mixer) to  handle this
	(176 . "XF86AudioRaiseVolume")))

;; Map keycodes to keysyms
(mapcar (lambda (pair)
	  (let* ((keycode (car pair))
		 (keysym  (cdr pair))
		 (format-dest nil)
		 (format-dest (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)))
	    (format format-dest "xmodmap -e 'keycode ~d = ~a'" keycode keysym)
	    (run-shell-command format-dest)
	  format-dest))
	*key-codes*)

;; Volume control
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")

;; Mute
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioMute") "exec amixer set Master toggle")
(define-key stumpwm:*root-map* (stumpwm:kbd "q") "exec killall python")
