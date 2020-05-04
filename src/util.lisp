(in-package #:discord-play-register)

(defun string-contains (needle haystack)
  (search needle haystack))

(defun string-contains-any (needles haystack)
  (loop for needle in needles thereis (string-contains needle haystack)))

;;;; https://sourceforge.net/p/cl-cookbook/patches/8/
(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while"
  (let ((lock-path (gensym))
        (lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
            (progn
              (loop
                 :for ,lock-file = (open ,lock-path :direction :output
                                         :if-exists nil
                                         :if-does-not-exist :create)
                 :until ,lock-file
                 :do (sleep ,(or interval 0.1))
                 :finally (close ,lock-file))
              ,@body)
         (ignore-errors
           (delete-file ,lock-path))))))

(defun strip (string)
  (string-trim (format nil "~%") (string-trim " " string)))
