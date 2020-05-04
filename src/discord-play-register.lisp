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
  (string-contains (format nil "<@!~A>" *bot-id*) message))

(defun process-message (message)
  (when-let ((command (get-appropriate-command message)))
    (execute-and-respond command message)))

(defun get-appropriate-command (message)
  (when-let ((cls (if (includes-bot-mention message)
                      (cond ((string-contains "status" message)
                             'StatusCommand)))))
    (make-instance cls)))
