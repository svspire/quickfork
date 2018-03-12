;;; quickfork.lisp
;;; 27-Feb-2018 SVS
;;; Quicklisp for teams

(in-package :quickfork)

#|
Functions needed:
(qf:repo-location :system) ; returns URL for repo for :system

Need a hook that produces a list of systems loaded whenever
(ql:quickload :system) runs.
For each system loaded, return:
Name of system.
Whether system was pulled across the net or not.
Where on the local file system the system was loaded from.
Whether the system was compiled or not.
|#


;;; Ensure that every time quickload is called, it will keep track of every
;;;  asdf system that it loads and/or compiles.

(defparameter cl-user::*compiled-systems* nil "Systems compiled by quickload")
(defparameter cl-user::*loaded-systems* nil "Systems loaded by quickload")
(defparameter cl-user::*installed-systems* nil "Releases installed (downloaded or reexpanded from ~
a downloaded archive) by Quicklisp")

(defmethod ql:quickload :before (systems &key verbose prompt explain
                                         &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (setf cl-user::*compiled-systems* nil
        cl-user::*loaded-systems* nil
        cl-user::*installed-systems* nil))

(defmethod ql:quickload :after (systems &key verbose prompt explain
                                         &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (format t "~%Systems compiled by QL: ")
  (if cl-user::*compiled-systems*
      (dolist (system cl-user::*compiled-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%Systems loaded by QL: ")
  (if cl-user::*loaded-systems*
      (dolist (system cl-user::*loaded-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%Systems installed by QL: ")
  (if cl-user::*installed-systems*
      (dolist (release cl-user::*installed-systems*)
        (format t "~% ~S" (ql-dist::project-name release)))
      (format t "(none)"))
  (format t "~%Inspect cl-user::*compiled-systems*, cl-user::*loaded-systems*, and ~
cl-user::*installed-systems* for more info."))

(defmethod asdf:perform :around ((op asdf::basic-compile-op) (file asdf:CL-SOURCE-FILE))
  (let* ((system (asdf:component-system file))
         (loc (ql:where-is-system system))
         (syslist nil))
    (when loc
      (setf syslist (list (asdf:component-name system)
                          loc))
      (unless (find (asdf:component-name system) cl-user::*compiled-systems*
                    :test 'equalp
                    :key 'car)
        (setf cl-user::*compiled-systems* (append cl-user::*compiled-systems* (list syslist))))))
  (call-next-method))

(defmethod asdf:perform :around ((op asdf::basic-load-op) (c asdf:system))
  (let ((loc (ql:where-is-system c))
        (syslist nil))
    (when loc
      (setf syslist (list (asdf:component-name c)
                          loc))
      (setf cl-user::*loaded-systems* (append cl-user::*loaded-systems* (list syslist))))
  (call-next-method)))

(defmethod ql-dist::install :after ((release ql-dist::release))
  (setf cl-user::*installed-systems* (append cl-user::*installed-systems* (list release))))

; you need to clone the repo at https://github.com/quicklisp/quicklisp-projects
;  to get this directory
(defparameter *projects-directory* "~/Lisp/third-party/quicklisp-projects/projects/"
  "Quicklisp projects directory")

(defvar *projects-database* nil "Database of all known Quicklisp projects including their name, their
type (e.g. :git, :mercurial, etc.) and their upstream repository location")

(defun wsp (char)
  (member char '(#\space #\tab #\return #\linefeed)))

(defun whitespace-partition (string &optional (start 0) (accum nil))
  "Return a list of substrings from string, where whitespace separates substrings."
  (cond ((zerop (length string))
         (nreverse accum))
        (t
         (let ((wspointer (position-if #'wsp string :start start)))
           (cond (wspointer
                  (cond ((= start wspointer) ; starts with whitespace
                         (setf start (position-if (complement #'wsp) string :start start))
                         (if start
                             (whitespace-partition string start accum)
                             (nreverse accum)))
                        (t
                         (whitespace-partition string (1+ wspointer) (cons (subseq string start wspointer)
                                                                           accum)))))
                 (t (nreverse (cons (subseq string start)
                                    accum))))))))


(defun populate-projects (&optional (projects-directory *projects-directory*))
  "Populate the projects database from the projects directory.
  Projects database allows us to look up the original repo for a given project."
  (flet ((get-source (dirpath)
           "Call on each individual project to return source data"
           (let ((source-path (merge-pathnames "source.txt"
                                               dirpath)))
             (when (probe-file source-path)
               (let ((raw-string nil)
                     (data nil))
                 (with-open-file (s source-path)
                   (setf raw-string (read-line s nil nil nil)))
                 (setf data (whitespace-partition raw-string))
                 (when data
                   (setf (first data)
                         (intern (string-upcase (first data))
                                 #.(find-package "KEYWORD"))))
                 data)))))
    
    (let ((dirpaths (uiop:subdirectories projects-directory)))
      (mapcar (lambda (dirpath)
                (cons (car (last (pathname-directory dirpath)))
                      (get-source dirpath)))
              dirpaths))))

(defun ensure-projects-database ()
  (unless *projects-database* (setf *projects-database* (populate-projects))))

(defun stats ()
  "Report number of projects per repo type, sorted most to least."
  (ensure-projects-database)
  (let ((tallies (make-hash-table))
        (alist nil))
    (flet ((tally (key)
             (if (gethash key tallies)
                 (incf (gethash key tallies))
                 (setf (gethash key tallies) 1))))
      (mapc (lambda (data)
              (tally (second data)))
            *projects-database*)
      (maphash (lambda (key value)
                 (setf alist (acons key value alist)))
               tallies)
      (sort alist #'> :key #'cdr))))

