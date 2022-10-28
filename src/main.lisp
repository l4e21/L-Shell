(defpackage l-shell
  (:use :cl)
  (:export #:repl))

(in-package :l-shell)

(defun random-of (xs)
  (nth (random (length xs)) xs))

(setf *welcomes*
      '("Welcome to L-Shell (Alpha) v0.2"
        "Have a spooky halloween!"
        "Remember that you love your job"
        "Here we go again"
        "Get LISPing already"
        ":D :D :D :D :D :D :D :D :D :D :D :D"
        "The path is difficult. Here, take this REPL!"))

;; We want some way to print data but for it to be directly accessible, so for ls, we return a plist. same for ff.

(defclass pretty-plist ()
  ((data
    :initarg :data
    :accessor data)
   (print-fn
    :initarg :print-fn
    :accessor print-fn)))

(defmethod print-object ((obj pretty-plist) stream)
  (print-unreadable-object (obj stream :type t)
    (funcall (print-fn obj) stream obj)))

(defun make-pplist (data print-fn)
  (make-instance 'pretty-plist
                 :data data
                 :print-fn print-fn))

(defmethod getp ((obj pretty-plist) key)
  (getf (data obj) key))


;; Flags
(setf __ "..")
(setf ~/ "~/")
(setf -s :search)
(setf -p :path)

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
    (make-pplist (list
                  :files ls-files
                  :dirs ls-dirs)
                 (lambda (stream obj)
                   (format stream "~%::FILES::~%~{~a~^~%~}~%~%::DIRECTORIES::~%~{~a~^~%~}~%" (mapcar #'filename (getp obj :files))
                           (mapcar #'dirname (getp obj :dirs)))))))


(defun ff (&key
             (path "")
             (search "")
             (acc (make-pplist
                   (list :files nil :dirs nil)
                   (lambda (stream obj)
                     (format stream "~%::FILES::~%~{~a~^~%~}~%~%" (getp obj :files))))))
  (let ((ls-files
          (remove-if-not
           (lambda (file)
             (cl-ppcre:scan search
                            (filename (namestring file))))
           (uiop:directory-files (get-real-path path))))
        (ls-dirs (uiop:subdirectories (get-real-path path))))
    (reduce
     (lambda (acc dir)
       (ff :path (concatenate 'string path
                              (if (and (coerce path 'list)
                                       (char= #\/ (first (last (coerce path 'list))))) "" "/")
                              (dirname dir))
           :search search
           :acc acc))
     ls-dirs
     :initial-value (make-pplist (list
                                  :files (append (getp acc :files)
                                                 ls-files)
                                  :dirs (append (getp acc :dirs)
                                                ls-dirs))
                                 (lambda (stream obj)
                                   (format stream "~%::FILES::~%~{~a~^~%~}~%~%" (getp obj :files)))))))

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
  (setf *random-state* (make-random-state t))
  (setf *running* t)
  (format t "~%~a~%" (random-of *welcomes*))
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
