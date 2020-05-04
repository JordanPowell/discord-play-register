;;;; discord-play-register.lisp

(in-package #:discord-play-register)

(defun load-bot-id ()
  (cdr (assoc :client--id (cl-json:decode-json-from-source (asdf:system-relative-pathname :discord-play-register "auth.json")))))

(defparameter *bot-id* (load-bot-id))

(defun main ()
  (process-message
   (with-output-to-string (stream)
     (loop for line = (read-line *standard-input* nil :eof) ; stream, no error, :eof value
        until (eq line :eof)
        do (format stream "~A~%" line)))))

(defun includes-bot-mention (message)
  (search (format nil "<@!~A>" *bot-id*) message))

(defun contains-trigger-words (messsage)
  nil)

(defun is-applicable-message (message)
  (or (includes-bot-mention message)
      (contains-trigger-words message)))

(defun process-message (message)
  (when (is-applicable-message message)
    (handle-applicable-message message)))

(defun handle-applicable-message (message)
  (format t "Handling ~A~%" message))
