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
  (and (tooter:in-reply-to-id status)
       ;;and there is only whitespace (not including mentions) in the status we were mentioned in
       (not (ppcre:scan "\S" (strip-mentions status)))))

(defun lizard-bot-p (account)
  (equal (tooter:id account) (tooter:id (tooter:account (glacier:bot-client *bot*)))))

(defun analyze-status-p (status)
  (and (not (glacier:no-bot-p (tooter:id (tooter:account status))))
       (or (not (glacier:bot-post-p status))
	   (lizard-bot-p (tooter:account status)))))

(defun format-mentions (stream status)
  (format stream "~{@~a ~}" (loop for mention being the elements of (tooter:mentions status)
				  unless (or (glacier:no-bot-p (tooter:id mention))
					     (equal (tooter:account-name mention)
						    (tooter:account-name (tooter:account
									  (glacier:bot-client *bot*)))))
				    collect (tooter:account-name mention))))

(defun random-from-list (list)
  (nth (random (length list)) list))

(defun random-prefix ()
  (random-from-list '("yocto" "zepto" "atto" "femto" "pico" "nano" "micro" "milli" "centi" "deci"
		      "" "deca" "hecto" "kilo" "kibi" "mega" "mebi" "giga" "gibi" "tera" "tebi" "peta" "pebi"
		      "exa" "exbi" "zetta" "zebi" "yotta" "yobi")))

(defun random-electric-unit ()
  (random-from-list '("amperes" "watts" "coulombs" "volts" "henrys" "ohms" "farads" "teslas")))

(defun format-analysis (stream status)
  (if (lizard-bot-p (tooter:account status))
      (format stream "A lizard didn't type that, I did, and it only took ~,2f ~a~a."
	      (random 999.99)
	      (random-prefix)
	      (random-electric-unit))
      (format stream "A lizard scuttling from key to key would have scuttled ~a cm to type this toot."
	      (lizard-length (print (concatenate 'string
						 (if (and (not (eql nil (tooter:spoiler-text status)))
							  (not (equal "" (tooter:spoiler-text status))))
						     (format nil "~a~a"
							     (tooter:spoiler-text status)
							     #\Tab))
						 (tooter:content status)))))))

(defun reply-with-analysis (status-to-reply-to status-to-analyze)
  (glacier:reply status-to-reply-to
		 (concatenate 'string
			      (format-mentions nil status-to-reply-to)
			      (format-analysis nil status-to-analyze))
		 :include-mentions nil))

(defun notified (notification)
  (print "Notified")
  (cond
    ((glacier:mention-p notification)
     (let* ((status-to-reply-to (tooter:status notification))
	    (analyze-ancestor (analyze-ancestor-p status-to-reply-to))
	    (status-to-analyze (print (if analyze-ancestor
					  (ancestor-status status-to-reply-to)
					  status-to-reply-to))))
       (if (analyze-status-p status-to-analyze)
	   (reply-with-analysis status-to-reply-to status-to-analyze))))))

(defun start-bot ()
  (setf *bot* (make-instance 'glacier:mastodon-bot
			     :config-file "~/common-lisp/lizard-bot/cfg.config"
			     :on-notification #'notified))
  (glacier:run-bot (*bot*)))
