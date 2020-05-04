(pushnew (parse-namestring (format nil "~a/" (sb-posix:getcwd))) asdf:*central-registry* :test 'equal)
(ql:quickload 'discord-play-register :silent t)
