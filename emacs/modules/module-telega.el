(pkg telega)

(defun telega-chatbuf-attach-username ()
  "Insert a username from the group into your message"
  (interactive)
  (let ((members (telega--searchChatMembers
		  telega-chatbuf--chat
		  "")))
    (let ((choice (telega-completing-read-user "Mention user: " members)))
      (telega-ins (telega-string-as-markup
		   (format "[%s](tg://user?id=%d)"
			   (concat "@" (plist-get choice :username))
			   (plist-get choice :id))
		   "markdown"
		   (apply-partially #'telega-markup-markdown-fmt 1))))))
(define-key telega-chat-mode-map (kbd "M-m") 'telega-chatbuf-attach-username)
