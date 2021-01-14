(in-package :stumpwm)
(set-prefix-key (kbd "C-i"))

(defcommand wallpaper () ()
  (let* ((wp-list (uiop:directory-files "/home/jorde/pic/wallpapers/"))
	 (wp (namestring (nth (random (length wp-list)) wp-list))))
    (run-shell-command (concatenate 'string "feh --bg-fill " wp))))

(defcommand push-window () ()
	    "Pushes a window into the next frame"
	    (progn
	      (mark)
	      (fnext)
	      (pull-marked)
	      (clear-window-marks)))

(defcommand vsplit-and-switch () ()
	"Splits vertically and switches to next window"
	(vsplit)
	(fnext))

(defcommand hsplit-and-switch () ()
	"Splits horizontally and switches to next window"
	(hsplit)
	(fnext))

(defun load-file (file)
  (load (concatenate 'string (stumpwm::getenv "HOME") "/.stumpwm.d/" file ".lisp")))
(load-file "shifting")
(load-file "defprog")

(defprog emacs "e" "emacsclient -a '' -c" '(:class "Emacs"))
(defprog firefox "f" "firefox" '(:class "Firefox"))
(defprog libreoffice "o" "libreoffice" '(:class "libreoffice"))

;; keybinds
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "N") "push-window")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "]") "next-in-frame")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "[") "prev-in-frame")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "n") "fnext")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "p") "fother")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "d") "exec dmenu_run")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "q") "delete")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "s") "vsplit-and-switch")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "S") "hsplit-and-switch")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "r") "remove")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "w") "iresize")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "l") "windowlist")

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "c")  "exec xfce4-screenshooter")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "f")  "float-this")

(defcommand email () ()
  (run-shell-command "emacsclient -a '' -c -e '(notmuch-my-search)'"))

(defcommand update-email () ()
  (uiop:run-program "offlineimap" :output NIL)
  (let ((nmail (uiop:run-program "notmuch new | tail -n1" :output :string)))
    (stumpwm:echo nmail)))

(defcommand volume-up () ()
  (uiop:run-program "pamixer --allow-boost -i 5"))
(defcommand volume-down () ()
  (uiop:run-program "pamixer --allow-boost -d 5"))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-u") "volume-down")
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-i") "volume-up")

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "E") "email")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "u") "update-email")

(setf *mouse-focus-policy*  :sloppy) ;; :click :ignore :sloppy -- Focus follows mouse
(set-win-bg-color "#0C1021")
(set-focus-color "red")
(set-unfocus-color "#0C1021")
(run-shell-command "xmodmap .Xmodmaprc")
