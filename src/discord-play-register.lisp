(in-package #:discord-play-register)

(defun load-bot-id ()
  (cdr (assoc :client--id (cl-json:decode-json-from-source (asdf:system-relative-pathname :discord-play-register "auth.json")))))

(defparameter *bot-id* (load-bot-id))

(defun main ()
  (process-message (apply #'make-instance 'Message (alist-plist (cl-json:decode-json-from-source *standard-input*)))))

(defun includes-bot-mention (message)
  (string-contains (format nil "<@!~A>" *bot-id*) message))

(defun process-message (message)
  (when-let ((command (get-appropriate-command message)))
    (execute-command command message)))

(defun send-message (format-string &rest args)
  (apply #'format t format-string args))

(defun get-appropriate-command (message)
  (let ((msg (message message)))
    (when-let ((cls (if (includes-bot-mention (message message))
                        (cond ((string-contains "status" msg)
                               'StatusCommand)
                              ((string-contains "ping" msg)
                               'PingCommand))
                        (cond ((string-contains-any *id-play-prefixes* (string-downcase msg))
                               'RecordWouldPlayCommand)))))
      (make-instance cls))))
