;;;; discord-play-register.asd

(asdf:defsystem #:discord-play-register
  :description "Play register for discord"
  :author "Jordan Powell"
  :license  "BSD 2-Clause License"
  :version "1.0.0"
  :serial t
  :depends-on (:alexandria :cl-json)
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/commands")
               (:file "src/discord-play-register")))
