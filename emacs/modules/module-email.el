(pkg notmuch)

(defun notmuch-my-search ()
  "Search for unread messages today"
  (interactive)
  (notmuch-search "date:today.. tag:unread"))

(provide 'module-email)
