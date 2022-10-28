(defpackage l-shell
  (:use :cl)
  (:export #:repl))

(in-package :l-shell)

;; Flags
(setf __ "..")
(setf ~/ "~/")
(setf -s :search)
(setf -p :path)
(setf -r :recur)

;; Deal with clunky logical pathing
(defun get-real-path (&optional (path nil))
  (uiop:truenamize
   (cond
     ((not path)
      (uiop:getcwd))
     ((or
       (and (>= (length path) 1) (string= (subseq path 0 1) "/"))
       (and (>= (length path) 2) (string= (subseq path 0 2) "~/")))
      path)
     (t
      (format nil "~{~^/~a~}" (append (rest (pathname-directory (uiop:getcwd))) (list path)))))))


;; Just the name with the extension
(defun filename (file)
  (concatenate 'string
               (pathname-name file)
               (if (pathname-type file)
                   ".")
               (pathname-type file)))

(defun dirname (dir)
  (concatenate 'string (first (last (pathname-directory dir))) "/"))


;; Run bash (returns the value as a string dump)
(defun b (program) (uiop:run-program program
                                     :output :string
                                     :ignore-error-status t))

(defun exec (fn &rest args) (b (format nil "~a ~{~a ~}" fn args)))

(defun exec-fn (fn args) (b (format nil "~a ~{~a ~}" fn args)))

(defun make (&rest args) (exec-fn "make" args))
(defun git (&rest args) (exec-fn "git" args))
(defun yarn (&rest args) (exec-fn "yarn" args))

(defun cd (&optional (path "~/"))
  (uiop:chdir (get-real-path path)))

(defun cwd () (uiop:getcwd))

(defun mk (path)
  (ensure-directories-exist
   (get-real-path
    (concatenate 'string
                 path
                 (if (string= "/" (subseq (reverse path) 0 1))
                     ""
                     "/")))))

(defun rm (path &key (search nil))
  (uiop:delete-file-if-exists (get-real-path path))
  (if search
      (mapcar #'uiop:delete-file-if-exists
              (uiop:truenamize
               (remove-if-not (lambda (file)
                                (cl-ppcre:scan search (namestring file)))
                              (uiop:directory-files (get-real-path path)))))
      ;; Be careful.
      (if (uiop:directory-exists-p (get-real-path path))
          (uiop:delete-directory-tree (get-real-path path) :validate t))))

(defclass ls ()
  ((files
    :initarg :files
    :accessor files)
   (dirs
    :initarg :dirs
    :accessor dirs)))

(defmethod print-object ((obj ls) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
               "~%::FILES::~%~{~a~^~%~}~%~%::DIRECTORIES::~%~{~a~^~%~}~%"
               (mapcar #'filename (files obj))
               (mapcar #'dirname (dirs obj)))))

(defun ls (&key (path "") (search ""))
  (let* ((ls-files
          (remove-if-not
           (lambda (file)
             (cl-ppcre:scan search
                     (filename (namestring file))))
           (uiop:directory-files (get-real-path path))))
         (ls-dirs
          (remove-if-not
           (lambda (dir)
             (cl-ppcre:scan search
                     (first (last (pathname-directory (namestring dir))))))
           (uiop:subdirectories (get-real-path path)))))
    (make-instance 'ls
                   :files ls-files
                   :dirs ls-dirs)))

(defclass ff (ls) ())

(defmethod print-object ((obj ff) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~%::FILES::~%~{~a~^~%~}~%~%" (files obj))))

(defun ff (&key
             (path "")
             (search "")
             (acc (make-instance 'ff :files nil :dirs nil)))
  (let ((result (ls :path path :search search)))
    (reduce
     (lambda (acc dir)
       (ff :path (concatenate 'string path (dirname dir))
           :search search
           :acc acc))
     (dirs result)
     :initial-value (make-instance 'ff
                                   :files (append (files acc)
                                                  (files result))
                                   :dirs (append (files acc)
                                                 (dirs result))))))

(defun touch (output &key (concat nil))
  (uiop:concatenate-files (mapcar #'get-real-path concat)
                          (get-real-path output)))

(defun cp (input output)
  (let ((from (get-real-path input))
        (to (get-real-path output)))
    (cond
      ;; If it user provided a "/" at end of input
      ((and (directory-exists-p from)
            (string= (subseq (reverse (namestring from)) 0 1) "/"))
       (let ((to
               (if (string= "/"
                            (subseq (reverse (namestring to)) 0 1))
                   to
                   (uiop:truenamize (concatenate 'string (namestring to) "/")))))
         (mk (namestring to))
         (mapcar (lambda (file)
                   (cp (namestring file)
                       (concatenate 'string
                                    (namestring to)
                                    (full-filename file))))
                 (uiop:directory-files from))
         (mapcar (lambda (dir)
                   (cp (namestring dir)
                       (concatenate 'string
                                    (namestring to)
                                    (first (last (pathname-directory dir)))
                                    "/")))
                 (uiop:subdirectories from))))

      ;; If it's a file but the user provided a directory to
      ((string= (subseq (reverse (namestring to)) 0 1) "/")
       (uiop:copy-file from (concatenate 'string (namestring to) (full-filename from))))
      ;; Both are files
      (t
       (uiop:copy-file from (namestring to))))))

;; (defun write ())

(defun repl ()
  (format t "~%L-Shell (Alpha) V0.2.~%")
  (setf *running* t)
  (loop :while *running* :do 
        (format t "#~{~A~^/~}> "
                (let ((cwd (rest (pathname-directory (get-real-path)))))
                  (cond
                    ((find "jam" cwd :test #'string=)
                     (cons "~" (cdr (cdr cwd))))
                    (t
                     cwd))))
    (finish-output)
    (handler-case
        (progn
          (format t "~a~%"
           (eval (read-from-string
                  (read-line))))
          (finish-output))
      (sb-sys:interactive-interrupt ()
        (progn
          (format *error-output* "~%Goodbye.~%~%")
          (setf *running* nil)))
      (error (c)
        (format t "~a~%" c)))))
