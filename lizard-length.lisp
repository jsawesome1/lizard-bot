(in-package :lizard-bot)

(defclass key ()
  ((width
    :initarg :width
    :initform 1
    :accessor width)))

(defclass char-key (key)
  ((basic-char
    :initarg :basic-char
    :accessor basic-char)
   (shift-char
    :initarg :shift-char
    :initform nil
    :accessor shift-char)
   (caps-lock-works
    :initarg :caps-lock
    :initform t
    :accessor caps-lock)))

(defmethod initialize-instance :after ((new-key char-key) &key)
  (with-slots (basic-char shift-char) new-key
    (if (and basic-char (not shift-char))
	(setf shift-char basic-char))))

(defclass modifier-key (key)
  ((modifier
    :initarg :modifier
    :accessor modifier)
   (toggle
    :initarg :toggle
    :initform nil
    :accessor toggle)))

(defmethod print-object :around ((object key) stream)
  (print-unreadable-object (object stream :type t)
    (call-next-method)))

(defmethod print-object ((object key) stream)
  (format stream "Width: ~a" (width object)))

(defun fchar (char)
  (format nil (if (member char
			  '(#\Backspace #\Tab #\Newline #\Space #\Page #\Linefeed #\Rubout)
			  :test #'eql)
		  "~s"
		  "~a")
	  char))

(defmethod print-object ((object char-key) stream)
  (with-slots (basic-char shift-char caps-lock-works) object
    (format stream "~a/~a Caps: ~a " (fchar basic-char) (fchar shift-char) caps-lock-works))
  (call-next-method))

(defmethod print-object ((object modifier-key) stream)
  (with-slots (modifier toggle) object
    (format stream "Modifier: ~a Toggle: ~a " modifier toggle))
  (call-next-method))

(defclass keyboard ()
  ((row-length
    :initarg row-length
    :initform 15
    :accessor row-length)
   (key-array
    :initform (make-array 108 :fill-pointer 0 :adjustable t)
    :reader key-array)
   (keys-down
    :initform nil
    :reader keys-down)
   (modifiers-active
    :initform (make-hash-table)
    :reader modifiers-active)
   (output
    :initform (make-array 10 :element-type 'character :fill-pointer 0 :adjustable t))))

(defmethod print-object ((object keyboard) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (row-length key-array) object
      (format stream "row-length: ~a, key-array: ~a" row-length key-array))))

(defgeneric add-key (keyboard key)
  (:documentation "Add a key to the keyboard"))

(defmethod add-key ((keyboard keyboard) (key key))
  (vector-push-extend key (key-array keyboard)))

(defgeneric typeablep (char-or-mod object)
  (:documentation "returns whether or not char can be typed with object"))

(defmethod typeablep ((char character) (object char-key))
  (or (equal (basic-char object) char)
      (and (equal (shift-char object) char)
	   (caps-lock object))))

(defmethod typeablep ((modifier symbol) (object modifier-key))
  (equal modifier (modifier object)))

(defmethod typeablep (any (object key))
  nil)

(defmethod typeablep (char (object keyboard))
  (some #'(lambda (key) (typeablep char key)) (key-array object)))

(defun get-index (keyboard char-or-mod)
  (position-if #'(lambda (key) (typeablep char-or-mod key))
	       (key-array keyboard)))

(defun get-position (keyboard char-or-mod)
  "Returns the position of the center of the key at key-index in keyboard's key-array"
  (loop for key being the elements of (key-array keyboard)
	with row-num = 0
	and current-row-length = 0
	do
	   (when (typeablep char-or-mod key)
	     (return (list (+ 1/2 row-num)
			   (+ current-row-length (/ (width key) 2)))))
	   (incf current-row-length (width key))
	   (when (< (row-length keyboard) current-row-length)
	     (setf current-row-length (width key))
	     (incf row-num))))

(defun key-at-position (keyboard position)
  "Returns the key at position on keyboard"
  (let ((position (list (floor (first position)) (second position))))
    (loop for key being the elements of (key-array keyboard)
	  with row-num = 0
	  and current-row-length = 0
	  do
	     (when (and (eql row-num (first position))
			(< current-row-length (second position) (+ current-row-length (width key))))
	       (return key))
	     (incf current-row-length (width key))
	     (when (< (row-length keyboard) current-row-length)
	       (setf current-row-length (width key))
	       (incf row-num)))))

(defun get-key (keyboard char-or-mod)
  (aref (key-array keyboard) (get-index keyboard char-or-mod)))

(defgeneric add-char-to-output (keyboard char)
  (:documentation "Add char to output string on keyboard, or delete if char is #\Backspace"))

(defmethod add-char-to-output ((keyboard keyboard) (char character))
  (with-slots (output) keyboard
    (if (eql #\Backspace char)
	(vector-pop output)
	(vector-push-extend char output 100))))

(defgeneric key-down (keyboard key)
  (:documentation "Presses key down on keyboard"))

(defmethod key-down ((keyboard keyboard) (key key))
  (with-slots (keys-down) keyboard
    (push key keys-down)))

(defun xor (a b) (not (eql (not a) (not b))))

(defmethod key-down ((keyboard keyboard) (key char-key))
  (with-slots (modifiers-active) keyboard
    (add-char-to-output keyboard
			(if (xor (gethash 'shift modifiers-active)
				 (if (caps-lock key)
				     (gethash 'caps-lock modifiers-active)
				     nil))
			    (shift-char key)
			    (basic-char key))))
  (call-next-method))

(defmethod key-down ((keyboard keyboard) (key modifier-key))
  (with-slots (modifiers-active) keyboard
    (setf (gethash (modifier key) modifiers-active) (if (toggle key)
							(not (gethash (modifier key) modifiers-active))
							t)))
  (call-next-method))

(defgeneric key-up (keyboard key)
  (:documentation "Releases key on keyboard"))

(defmethod key-up ((keyboard keyboard) (key key))
  (with-slots (keys-down) keyboard
    (setf keys-down (remove key keys-down))))

(defmethod key-up ((keyboard keyboard) (key modifier-key))
  (with-slots (modifiers-active) keyboard
    (unless (toggle key)
      (setf (gethash (modifier key) modifiers-active) nil)))
  (call-next-method))

(defun key-press (keyboard key)
  (key-down keyboard key)
  (key-up keyboard key))

(defgeneric send-output (keyboard)
  (:documentation "Returns the output slot of keyboard and clears it"))

(defmethod send-output ((keyboard keyboard))
  (with-slots (output) keyboard
    (prog1 output
      (setf output (make-array 10 :element-type 'character :fill-pointer 0 :adjustable t)))))

(defun add-all-char-keys (keyboard char-keys &optional (caps-lock-works t))
  (loop for char-key in char-keys
	do
	   (add-key keyboard (make-instance 'char-key
					    :basic-char (first char-key)
					    :shift-char (second char-key)
					    :caps-lock caps-lock-works))))

(defun add-number-row (keyboard)
  (add-all-char-keys keyboard
		     '((#\` #\~)
		       (#\1 #\!)
		       (#\2 #\@)
		       (#\3 #\#)
		       (#\4 #\$)
		       (#\5 #\%)
		       (#\6 #\^)
		       (#\7 #\&)
		       (#\8 #\*)
		       (#\9 #\()
		       (#\0 #\))
		       (#\- #\_)
		       (#\= #\+))
		     nil))

(defun add-qwerty-top-row (keyboard)
  (add-all-char-keys keyboard
		     '((#\q #\Q)
		       (#\w #\W)
		       (#\e #\E)
		       (#\r #\R)
		       (#\t #\T)
		       (#\y #\Y)
		       (#\u #\U)
		       (#\i #\I)
		       (#\o #\O)
		       (#\p #\P))))

(defun add-qwerty-mid-row (keyboard)
  (add-all-char-keys keyboard
		     '((#\a #\A)
		       (#\s #\S)
		       (#\d #\D)
		       (#\f #\F)
		       (#\g #\G)
		       (#\h #\H)
		       (#\j #\J)
		       (#\k #\K)
		       (#\l #\L))))

(defun add-qwerty-low-row (keyboard)
  (add-all-char-keys keyboard
		     '((#\z #\Z)
		       (#\x #\X)
		       (#\c #\C)
		       (#\v #\V)
		       (#\b #\B)
		       (#\n #\N)
		       (#\m #\m))))

(defun default-keyboard ()
  (let ((keeb (make-instance 'keyboard )))
    ;; Row 0
    (add-number-row keeb)
    (add-key keeb (make-instance 'char-key :basic-char #\Backspace :width 2))
    ;; Row 1
    (add-key keeb (make-instance 'char-key :basic-char #\Tab  :width 1.5))
    (add-qwerty-top-row keeb)
    (add-all-char-keys keeb '((#\[ #\{)
			      (#\] #\}))
		       nil)
    (add-key keeb (make-instance 'char-key :basic-char #\\ :shift-char #\| :width 1.5 :caps-lock nil))
    ;; Row 2
    (add-key keeb (make-instance 'modifier-key :modifier 'caps-lock :toggle t :width 1.75))
    (add-qwerty-mid-row keeb)
    (add-all-char-keys keeb '((#\; #\:)
			      (#\' #\"))
		       nil)
    (add-key keeb (make-instance 'char-key :basic-char #\Newline :width 2.25))
    ;; Row 3
    (add-key keeb (make-instance 'modifier-key :modifier 'shift :toggle nil :width 2.25))
    (add-qwerty-low-row keeb)
    (add-all-char-keys keeb '((#\, #\<)
			      (#\. #\>)
			      (#\/ #\?))
		       nil)
    (add-key keeb (make-instance 'modifier-key :modifier 'shift :toggle nil :width 2.75))
    ;; Row 4
    (add-key keeb (make-instance 'modifier-key :modifier 'control :width 1.5))
    (add-key keeb (make-instance 'modifier-key :modifier 'super :width 1.25))
    (add-key keeb (make-instance 'modifier-key :modifier 'meta :width 1.25))
    (add-key keeb (make-instance 'char-key :basic-char #\Space :width 5.75))
    (add-key keeb (make-instance 'modifier-key :modifier 'meta  :width 1.25))
    (add-key keeb (make-instance 'modifier-key :modifier 'super :width 1.25))
    (add-key keeb (make-instance 'key :width 1.25))  ;"Menu" key
    (add-key keeb (make-instance 'modifier-key :modifier 'control :width 1.5))
    keeb ))

(defun distance (p1 p2)
  (sqrt (+ (expt (- (first p2) (first p1)) 2) (expt (- (second p2) (second p1)) 2))))

(defun shift-char-typed (keyboard key)
  (xor (gethash 'shift (modifiers-active keyboard))
       (if (caps-lock key)
	   (gethash 'caps-lock (modifiers-active keyboard))
	   nil)))

(defun ramp (x)
  (if (< 0 x)
      x
      0))

(defun lizard-length (string &key (user (make-instance 'user)))
  "Returns (values units-scuttled time-taken vertical-gain steps-taken). If there isn't information supplied to determine the value of time-taken, vertical-gain, or steps taken, nil is returned instead"
  (let ((keyboard (keyboard user))
	(position (starting-position user))
	(units-scuttled 0)
	(time (if (or (move-speed user) (key-press-time user)) 0 nil))
	(y-units-scuttled (if (keyboard-tilt user) 0 nil))
	(steps-taken (if (step-size user) 0 nil)))
    (flet ((move (new-position)
	     (let ((segment-length (distance position new-position)))
	       (incf units-scuttled segment-length)
	       (when (keyboard-tilt user)
		 (incf y-units-scuttled (ramp (- (first new-position) (first position)))))
	       (when (move-speed user)
		 (incf time (/ segment-length (move-speed user))))
	       (when (step-size user)
		 (incf steps-taken (ceiling (/ (* segment-length (keyboard-unit-size user))
					       (step-size user))))))
	     (setf position new-position))
	   (press ()
	     (key-press keyboard (key-at-position keyboard position))
	     (when (key-press-time user)
	       (incf time (key-press-time user)))))
      (loop for char being the elements in string
	    do (when (typeablep char keyboard)
		 (when (and (shift-char-typed keyboard (get-key keyboard char))
			    (not (eql char (shift-char (get-key keyboard char)))))
		   (move (get-position keyboard 'caps-lock))
		   (press))
		 (let ((next-pos (get-position keyboard char)))
		   (move next-pos)
		   (press)))
	    finally (return (values (* units-scuttled (keyboard-unit-size user))
				    (list (cons :units-moved (* units-scuttled (keyboard-unit-size user)))
					  (cons :time-taken time)
					  (cons :vertical-gain (when (keyboard-tilt user)
								 (* y-units-scuttled
								    (keyboard-unit-size user)
								    (sin (keyboard-tilt user)))))
					  (cons :steps-taken steps-taken))))))))
