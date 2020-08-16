(defsystem :lizard-bot
  :description "mastodon bot that replies with the distance scuttled by a lizard typing a message"
  :author "Jacob Singleton"
  :license "MIT"
  :depends-on (:glacier)
  :serial t
  :components ((:file "package")
	       (:file "lizard-length")
	       (:static-file "cfg.config")
	       (:file "bot")))
