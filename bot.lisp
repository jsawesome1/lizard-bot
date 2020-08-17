(in-package :lizard-bot)

(defparameter *bot* nil)

(defun strip-mentions (status)
  (loop for mention being the elements of (tooter:mentions status)
	with content = (tooter:content status)
	do (setf content (str:replace-all (tooter:account-name mention) content ""))
	finally (return content)))

(defun ancestor-status (status)
  (tooter:find-status (glacier:bot-client *bot*) (tooter:in-reply-to-id status)))

(defun analyze-ancestor-p (status)
  ;;if  an ancestor status exists
  (if (and (tooter:in-reply-to-id status)
	   ;;and there is only whitespace (not including mentions) in the status we were mentioned in
	   (not (ppcre:scan "\S" (strip-mentions status)))
	   ;; and the ancestor status has non-mention content
	   (ppcre:scan "\S" (strip-mentions (ancestor-status status))))
      (ancestor-status status)
      nil))

(defun analyze-status-p (status)
  (and (not (glacier:no-bot-p (tooter:id (tooter:account status))))
       (not (glacier:bot-post-p status))))

(defun format-mentions (stream status)
  (format stream "~{~a ~}" (loop for mention being the elements of (tooter:mentions status)
				 unless (glacier:no-bot-p (tooter:id mention))
				   collect (tooter:account-name mention))))

(defun reply-with-analysis (status-to-reply-to status-to-analyze)
  (glacier:reply status-to-reply-to
		 (format nil "~aA lizard scuttling from key to key would have scuttled ~a cm to type this toot."
			 (format-mentions nil status-to-reply-to)
			 (lizard-length (concatenate 'string
						     (tooter:spoiler-text status-to-analyze)
						     (tooter:content status-to-analyze))))
		 :include-mentions nil))

(defun notified (notification)
  (print "Notified")
  (cond
    ((glacier:mention-p notification)
     (let* ((status-to-reply-to (tooter:status notification))
	    (analyze-ancestor (analyze-ancestor-p status-to-reply-to))
	    (status-to-analyze (print (if analyze-ancestor
					  analyze-ancestor
					  status-to-reply-to))))
       (if (analyze-status-p (or analyze-ancestor
				 status-to-reply-to))
	   (reply-with-analysis status-to-reply-to status-to-analyze))))))

(defun start-bot ()
  (setf *bot* (make-instance 'glacier:mastodon-bot
			     :config-file "~/common-lisp/lizard-bot/cfg.config"
			     :on-notification #'notified))
  (glacier:run-bot (*bot*)))
