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

(defun repo-location (project-name)
  "Look up the repo for a given project name"
  (ensure-projects-database)
  (when (symbolp project-name)
    (setf project-name (string-downcase (symbol-name project-name))))
  (find project-name *projects-database* :key 'car :test 'string-equal))