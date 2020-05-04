#!/usr/bin/sbcl --script
(loop for line = (read-line *standard-input* nil :eof) ; stream, no error, :eof value
      until (eq line :eof)
      do (format t "red~A~%" line))
(format t "Hey, ~a~%" (+ 1 2))
(format t "Hey, ~a~%" (+ 1 3))

