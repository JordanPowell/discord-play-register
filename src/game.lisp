(in-package #:discord-play-register)

(defparameter *id-play-prefixes*
  '("id play" "i'd play" "id paly" "i'd paly" "idplay" "i'dplay"))

(defun extract-game-name (string)
  (loop for prefix in *id-play-prefixes* do
       (when-let ((idx (search prefix (string-downcase string))))
         (let ((subs (subseq string (+ idx (length prefix)))))
           (return (strip (subseq subs 0 (or (length subs) (search subs #\Space)))))))))

(defun get-possible-names (game)
  (cons (name game) (aliases game)))

(defun extract-game (string)
  (when-let ((game-name (extract-game-name string)))
    (find-game-by-name game-name)))

(defun find-game-by-name (game-name)
  (or (find game-name *games*
            :key #'get-possible-names
            :test #'(lambda (s names) (member s names :test #'equalp)))
      (make-instance 'Game :name game-name)))

(defparameter *games* nil)

(defclass Game ()
  ((name :initarg :name :accessor name)
   (aliases :initarg :aliases :accessor aliases :initform nil)
   (min-players :initarg :min-players :accessor min-players :initform 1)
   (max-players :initarg :max-players :accessor max-players :initform 100)))

(defmacro define-game (symbol &key name aliases min-players max-players)
  (declare (ignore symbol))
  `(push (make-instance 'Game :name ,name :aliases ,aliases :min-players ,min-players :max-players ,max-players) *games*))


(define-game CS
    :name "CS:GO"
    :aliases '("cs" "csgo" "counterstrike" "counter strike global offensive")
    :min-players 5
    :max-players 5)

(define-game RocketLeague
    :name "Rocket League"
    :aliases '("rl" "rocketleague")
    :min-players 2
    :max-players 3)

(define-game Test
    :name "Test"
    :aliases '("tst")
    :max-players 1
    :min-players 1)

