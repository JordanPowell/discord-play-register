(in-package #:discord-play-register)

(defclass Command ()
  ())

(defmethod execute-and-respond ((command Command) message)
  (when-let ((response (get-response command message)))
    (format t "~a" response)))

(defgeneric get-response (command params))


(defclass StatusCommand (Command)
  ())

(defmethod get-response ((command StatusCommand) message)
  (format nil "Bot is alive~%"))
