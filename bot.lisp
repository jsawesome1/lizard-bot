(in-package :lizard-bot)

(defclass lizard-bot (glacier:mastodon-bot)
  ((accounts
    :initarg :accounts
    :initform (make-hash-table :test 'equal)
    :accessor accounts
    :documentation "A hash table to store all accounts that are subsribed to us. KEY is the account-name (username and domain). VALUE is an instance of account")))

(defclass account (tooter:account)
  ((users
    :initarg :users
    :initform nil
    :accessor users
    :documentation "A list of all users that might post to this account. If no signatures match, fallback by attributing the post to (first users)")))

(defclass user ()
  ((name
    :initarg :name
    :initform nil
    :accessor name
    :documentation "A name for the user to use to refer to this object")
   (accounts
    :initarg :accounts
    :initform nil
    :accessor accounts
    :documentation "A list of all accounts this user might post from")
   (primary-account-p
    :initarg :primary-account-p
    :initform t
    :accessor primary-account-p
    :documentation "If t, summaries will only mention the primary account (first accounts)")
   (signature ;use cl-str:containsp
    :initarg :signature
    :initform nil
    :accessor signature
    :documentation "A (non-regex) string that is matched against status contents (case insensitively) to attribute statuses to this user")
   (summary-period
    :initarg :summary-period
    :initform nil
    :accessor summary-period
    :documentation "The time between us posting summaries for this user, respresented as (integer . :time)")
   (last-summary-sent
    :initarg :last-summary-sent
    :initform nil
    :accessor last-summary-sent
    :documentation "The time the bot last sent the user a summary, as a Common Lisp Universal Time")
   (ignore-p
    :initarg :ignore-p
    :initform nil
    :accessor ignore-p
    :documentation "If t, don't respond or analyze statuses from this user")
   (animal
    :initarg :animal
    :accessor animal
    :documentation "The name of the type of animal this user represents")
   (animal-supplied-p
    :accessor animal-supplied-p
    :documentation "If the user supplied an animal, we can skip the 'if' when we talk about what type of animal typed the status")
   (verb
    :initarg :verb
    :accessor verb
    :documentation "The past tense form of the verb used to describe how the user moved from key to key")
   (verb-supplied-p
    :accessor verb-supplied-p
    :documentation "See animal-supplied-p")
   (keyboard
    :initarg :keyboard
    :initform (default-keyboard)
    :accessor keyboard
    :documentation "A model of the keyboard the user types on")
   (keyboard-unit-size
    :initarg :keyboard-unit-size
    :initform 1.9166666
    :accessor keyboard-unit-size
    :documentation "The width of a standard key on keyboard in length-units")
   (starting-position
    :initarg :starting-position
    :accessor starting-position
    :documentation "A position on keyboard in the form of (row column) in units of key-size (1 unit to the right is one standard-width key) with (0 0) at the top left of the keyboard, row increasing down, and column increasing to the right")
   (move-speed
    :initarg :move-speed
    :initform nil
    :accessor move-speed
    :documentation "A speed in units of length-unit per second")
   (key-press-time
    :initarg :key-press-time
    :initform nil
    :accessor key-press-time
    :documentation "The time it takes to press a key in seconds")
   (keyboard-tilt
    :initarg :keyboard-tilt
    :initform nil
    :accessor keyboard-tilt
    :documentation "The angle of the keyboard above horizontal in radians")
   (step-size
    :initarg :step-size
    :initform nil
    :accessor step-size
    :documentation "Average step size, in units of length-unit")
   (length-unit
    :initarg :length-unit
    :initform "cm"
    :accessor length-unit
    :documentation "The unit of length of the user's choice")
   (updated-length-unit
    :initform nil
    :accessor updated-length-unit
    :documentation "The unit to be used after the next summary, so calculations don't need to be redone")
   (stats-table
    :initarg :stats-table
    :initform (make-hash-table :test 'equal :synchronized t)
    :accessor stats-table
    :documentation "A hash table mapping status IDs onto alists of stats about that status")))

(defmethod initialize-instance :after ((user user) &rest initargs &key animal verb &allow-other-keys)
  (declare (ignore initargs))
  (if animal
      (progn (setf (animal user) animal)
	     (setf (animal-supplied-p user) t))
      (progn (setf (animal user) "lizard")
	     (setf (animal-supplied-p user) nil)))
  (if verb
      (progn (setf (verb user) verb)
	     (setf (verb-supplied-p user) t))
      (progn (setf (verb user) "scuttled")
	     (setf (verb-supplied-p user) nil))))

(defmethod initialize-instance :after ((user user) &rest initargs &key name starting-position &allow-other-keys)
  (declare (ignore initargs))
  (when (and (not name)
	     (accounts user))
    (setf (name user) (tooter:account-name (first (accounts user)))))
  (unless starting-position
    (setf (starting-position user) (list 4.5 (/ (row-length (keyboard user)) 2)))))

(defparameter *bot* nil)

(defun keywordize (string)
  (intern (string-upcase string) :keyword))

(define-condition configuration-error (parse-error)
  ((advice-to-user :initarg :advice
		   :initform nil
		   :reader advice)))

(defun find-interval (text)
  (multiple-value-bind (whole-match registers)
      (ppcre:scan-to-strings "(\\d+)\\s+((?:day|week|month)s?)" text)
    (unless whole-match
      (error 'configuration-error :advice "Please specify an interval in the form of an integer followed by the duration in days, weeks, or months. For example: \"3 days\" or \"1 week\"."))
    (let* ((amount (parse-integer (aref registers 0)))
	   (duration (keywordize (aref registers 1))))
      (cons amount duration))))

(defun interval-to-seconds (interval)
  (let ((amount (car interval))
	(duration (cdr interval)))
    (* amount (case duration
		((:day :days) (* 60 60 24))
		((:week :weeks) (* 60 60 24 7))
		((:month :months) (* 60 60 24 (/ 365 12)))
		(t (error "~S is not a keyword for day(s) week(s) or month(s)" duration))))))

(defun attribute-status (status)
  (let ((account (gethash (tooter:account-name (tooter:account status)) (accounts *bot*))))
    (when account
      (loop for user in (users account)
	    do (if (str:containsp (signature user) (tooter:content status))
		   (return-from attribute-status user)))
      (first (users account)))))

(defun ensure-account (account)
  (unless (gethash (tooter:account-name account) (accounts *bot*))
    (let* ((new-account (change-class account 'account))
	   (new-user (make-instance 'user :name (tooter:display-name account))))
      (setf (users new-account) (list new-user)
	    (accounts new-user) (list new-account))
      (setf (gethash (tooter:account-name account) (accounts *bot*)) new-account))))

(defun subscribe (status)
  "This command subscribes the bot to whomever called it"
  (print "Subscribing")
  (ensure-account (tooter:account status))
  (handler-case (let ((current-user (attribute-status status))
		      (interval (find-interval (tooter:content status))))
		  (setf (summary-period current-user) interval)
		  (glacier:reply status
				 (format nil "You've been subscribed to receive summaries every ~a ~a"
					 (car interval)
					 (cdr interval))))
    (configuration-error (err)
      (glacier:reply status (format nil "Something went wrong and you weren't subscribed. ~a" (advice err))))))

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

(defun analyze-status-p (status)
  (and (not (glacier:no-bot-p (tooter:id (tooter:account status))))
       (or (not (glacier:bot-post-p status))
	   (glacier:bot-post-p status))))

(defun format-mentions (stream status)
  (format stream "~{@~a ~}" (loop for mention being the elements of (tooter:mentions status)
				  unless (or (glacier:no-bot-p (tooter:id mention))
					     (equal (tooter:account-name mention)
						    (tooter:account-name (tooter:account
									  (glacier:bot-client *bot*)))))
				    collect (tooter:account-name mention))))

(defun random-from-list (list)
  (nth (random (length list)) list))

(defun random-adverb ()
  (random-from-list (list
		     ""
		     (random-from-list '("just " "exactly " "almost " "only " "about " "around " "approximately " "not even"))
		     (format nil "a ~a ~a "
			     (random-from-list '("bit" "little" "tad" "smidge" "smidgen"))
			     (random-from-list '("over" "under" "above" "below" "more than" "less than"))))))

(defun random-prefix ()
  (random-from-list '("yocto" "zepto" "atto" "femto" "pico" "nano" "micro" "milli" "centi" "deci"
		      "" "deca" "hecto" "kilo" "kibi" "mega" "mebi" "giga" "gibi" "tera" "tebi" "peta" "pebi"
		      "exa" "exbi" "zetta" "zebi" "yotta" "yobi")))

(defun random-electric-unit ()
  (random-from-list '("amperes" "watts" "coulombs" "volts" "henrys" "ohms" "farads" "teslas")))

(defun format-analysis (stream status)
  (if (glacier:bot-post-p status)
      (format stream "A lizard didn't type that, I did, and it took ~a~,2f ~a~a."
	      (random-adverb)
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
		 :include-mentions t))

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
  (setf *bot* (make-instance 'lizard-bot
			     :config-file "~/common-lisp/lizard-bot/cfg.config"
			     :on-notification #'notified ))
  (glacier:add-command "subscribe" #'subscribe *bot*)
  (glacier:run-bot (*bot*)))
