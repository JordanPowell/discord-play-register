(in-package #:discord-play-register)

(defclass Message ()
  ((user :accessor user :initarg :user)
   (user-id :accessor user-id :initarg :user--id)
   (channel-id :accessor channel-id :initarg :channel--id)
   (message :accessor message :initarg :message)))
