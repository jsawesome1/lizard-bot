(in-package :lizard-bot)

(defparameter *bot* (make-instance 'glacier:mastodon-bot :config-file "cfg.config"))

(defun cordial-reply (status)
  (glacier:reply status (format nil "hi, ~a" (tooter:display-name (tooter:account status)))))

(defun start-bot ()
  (glacier:add-command "hello" #'cordial-reply :add-prefix t)
  (glacier:run-bot (*bot*)))
