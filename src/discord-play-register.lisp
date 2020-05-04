;;;; discord-play-register.lisp

(in-package #:discord-play-register)

(defun main ()
  (process-message
   (with-output-to-string (stream)
     (loop for line = (read-line *standard-input* nil :eof) ; stream, no error, :eof value
        until (eq line :eof)
        do (format stream "~A~%" line)))))

(defun process-message (message)
  (format t "Got message ~a~%" message))
