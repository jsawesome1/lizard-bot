(in-package :lizard-bot)

(defun notified (notification)
  (cond
    ((glacier:mention-p notification)
     (let ((status (tooter:status notification)))
       (glacier:reply status
		      (format nil "A lizard scuttling from key to key would have scuttled ~a cm to type this toot."
			      (lizard-length (tooter:content status)))
		      :visibility (tooter:visibility status))))))

(defparameter *bot* (make-instance 'glacier:mastodon-bot
				   :config-file "cfg.config"
				   :on-notification #'notified))

(defun start-bot ()
  (glacier:run-bot (*bot*)))
