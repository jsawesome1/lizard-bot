(in-package :common-lisp-user)

(defpackage :lizard-bot
  (:use :common-lisp)
  (:export
   :key
   :char-key
   :modifier-key
   :keyboard
   :add-key
   :add-all-char-ckeys
   :add-number-row
   :add-qwerty-top-row
   :add-qwerty-mid-row
   :add-qwerty-low-row
   :default-keyboard
   :lizard-length))
