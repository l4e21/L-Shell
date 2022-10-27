(defpackage l-shell
  (:use :cl)
  (:export #:repl))

(in-package :l-shell)

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

(defun full-filename (file)
  (concatenate 'string
               (pathname-name file)
               (if (pathname-type file)
                   ".")
               (pathname-type file)))


(defun b (program) (uiop:run-program program :output :string))

(defun cd (&optional (path "~/")) (uiop:chdir (get-real-path path)))

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
                                (search search (namestring file)))
                              (uiop:directory-files (get-real-path path)))))
      ;; Be careful.
      (if (uiop:directory-exists-p (get-real-path path))
          (uiop:delete-directory-tree (get-real-path path) :validate t))))

(defun ls (&optional (path nil) &key (search ""))
  (format nil "::FILES::~%~{~a~^~%~}~%~%::DIRECTORIES::~%~{~a~^~%~}~%"
          ;; files
          (mapcar #'full-filename
           (remove-if-not
            (lambda (file)
              (search search (namestring file)))
            (uiop:directory-files (get-real-path path))))
          ;; directories
          (mapcar (lambda (dir)
                    (concatenate 'string
                                 (first (last (pathname-directory dir)))
                                 "/"))
           (remove-if-not
            (lambda (dir)
              (search search (namestring dir)))
            (uiop:subdirectories (get-real-path path))))))

(defun touch (output &key (concat nil))
  (uiop:concatenate-files (mapcar #'get-real-path concat)
                          (get-real-path output)))

(defun cp (input output)
  (let ((from (get-real-path input))
        (to (get-real-path output)))
    (cond
      ;; If it user provided a "/" at end of input
      ((string= (subseq (reverse (namestring from)) 0 1) "/")
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
       (copy-file from (concatenate 'string (namestring to) (full-filename from))))
      ;; Both are files
      (t
       (copy-file from (namestring to))))))

(defun repl ()
  (loop :do
    (format t "~{~A~^/~}> " (nthcdr 2 (pathname-directory (uiop:getcwd))))
    (finish-output)
    (handler-case (format t "~a~%" (eval (read-from-string
                                          (read-line))))
      (error (c)
        (format t "~a~%" c)))))
