(in-package #:discord-play-register)

(defstruct would-play user user-id game recorded-at expires-at)

(defun human-expiry (would-play)
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (would-play-expires-at would-play))
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defparameter *default-expiry* (* 60 60 2))
(defparameter *db-lockfile* (asdf:system-relative-pathname :discord-play-register "db.lock"))
(defparameter *db-file* (asdf:system-relative-pathname :discord-play-register "db.db"))

(defun expired (would-play)
  (> (get-universal-time) (would-play-expires-at would-play)))

(defun read-db ()
  (with-open-file (stream *db-file* :direction :input :if-does-not-exist :create)
    (read stream nil nil)))

(defun write-db (db)
  (with-open-file (stream *db-file* :direction :output :if-does-not-exist :create :if-exists :supersede)
    (prin1 db stream)))

(defmacro modify-db ((bind-var) &body body)
  `(with-file-lock (*db-lockfile*) 
     (let ((,bind-var (read-db)))
       (write-db (progn ,@body)))))

(defun record-would-play (&key user user-id game)
  (let ((wp (make-would-play
             :user user
             :user-id user-id
             :game (name game)
             :recorded-at (get-universal-time)
             :expires-at (+ (get-universal-time) *default-expiry*))))
    (modify-db (db)
               (remove-if #'expired (pushnew wp db
                                             :test #'(lambda (a b)
                                                       (and (equalp (would-play-user-id a) (would-play-user-id b))
                                                            (equalp (would-play-game a) (would-play-game b)))))))
    wp))


(defun get-available-players (game)
  (mapcar #'would-play-user (sort (remove-if-not #'(lambda (g) (equalp (name game) g)) (read-db) :key #'would-play-game)
                                  #'> :key #'would-play-recorded-at)))

(defun get-player-ids (players)
  (remove-duplicates
   (mapcar #'would-play-user-id (loop for wp in (read-db) when (member (would-play-user wp) players :test #'equalp)
                                   collect wp))
   :test #'equalp))
