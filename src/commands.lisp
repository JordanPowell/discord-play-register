(in-package #:discord-play-register)


(defclass Command ()
  ())

(defgeneric execute-command (command params))



(defclass RecordWouldPlayCommand (Command)
  ())

(defmethod execute-command ((command RecordWouldPlayCommand) params)
  (let ((game (extract-game (message params))))
    (let ((wp (record-would-play :user (user params) :user-id (user-id params) :game game)))
      (send-message "~A would play ~A (exp: ~a)~%" (user params) (name game) (human-expiry wp))
      (sleep 0.2)
      (send-ready-to-play-message-if-ready game))))


(defun is-ready-to-play (game)
  (when-let ((players (get-available-players game)))
    (>= (length players) (min-players game))))

(defmethod send-ready-to-play-message-if-ready (game)
  (when (is-ready-to-play game)
    (let ((players (get-available-players game)))
      (if (>= (length players) (max-players game))
          (send-message "Game ready! Ready to play ~A with ~{~a~^, ~}~%@ me with 'ping ~a' to @ the players~%" (name game) players (name game))
          (when (>= (length players) (min-players game))
            (send-message "Game potentially ready! Able to play ~A with ~{~a~^, ~}~%@ me with 'ping ~a' to @ the players~%" (name game) players (name game)))))))
      

(defclass StatusCommand (Command)
  ())

(defmethod execute-command ((command StatusCommand) params)
  (send-message "Bot is alive~%"))


(defclass PingCommand (Command)
  ())

(defmethod execute-command ((command PingCommand) params)
  (let ((game (find-game-by-name (strip (subseq (message params) (+ (search "ping" (message params)) 4))))))
    (when (is-ready-to-play game)
      (format t "~{~a~^, ~} we are ready to play ~a~%" (mapcar #'make-mention (get-player-ids (get-available-players game))) (name game)))))

(defun make-mention (user-id)
  (format nil "<@!~a>" user-id))
