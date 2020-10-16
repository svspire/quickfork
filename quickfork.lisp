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

(defmethod repo-location (project-name)
  "Look up the repo for a given project name"
  (ensure-projects-database)
  (when (symbolp project-name)
    (setf project-name (string-downcase (symbol-name project-name))))
  (find project-name *projects-database* :key 'car :test 'string-equal))

(defmethod repo-location ((project asdf::system))
  (repo-location (asdf::component-name project)))

(defun direct-dependencies (project-name)
  "Return the direct dependencies of project"
  (loop with main-system = (asdf:find-system project-name)
        with dependencies = (asdf:system-depends-on main-system)
        for dependency in dependencies
        for dep-system = (asdf::resolve-dependency-spec project-name dependency)
        when dep-system
          collect dep-system))

(defun %all-dependencies (project-list accum-table)
  (when project-list
    (mapc (lambda (project)
            (unless (gethash project accum-table)
              (setf (gethash project accum-table) t)
              (%all-dependencies (direct-dependencies project) accum-table)))
          project-list))
  (loop for k being each hash-key of accum-table collect k))

(defun all-dependencies (project-name)
  "Return a set of the names of all the dependencies of project."
  (let ((accum (make-hash-table :test #'equalp)))
    (%all-dependencies (list project-name) accum)))

; (ql:quickload :rcl) ; to retrieve all dependencies of rcl from the network as needed
; (all-dependencies :rcl) ; to get a list of all the dependencies of rcl
; (mapcar 'repo-location (all-dependencies :rcl)) ; to get a list of all the repos you'll need to fork.

(defun ends-with (subseq seq)
  "Returns t if seq ends with subseq"
  (let ((lsubseq (length subseq))
        (lseq (length seq)))
    (and (> lsubseq 0)
         (> lseq 0)
         (>= lseq lsubseq)
         (let ((pos (search subseq seq :from-end t)))
           (and pos (= pos (- lseq lsubseq)))))))

(defun make-clone-commands (project-name &optional clone-args)
  "Prints git clone commands for all dependencies of project that are of type :GIT.
   clone-args -- if non-nil -- is assumed to be a string such as \"--mirror\" which
   will be output as arguments to the git clone commands"
  (flet ((git? (loc)
           (eq :GIT (second loc)))
         (ensure-ends-with-git (string)
           (if (ends-with ".git" string)
               string
               (concatenate 'string string ".git"))))

    (let* ((locs (mapcar 'repo-location (all-dependencies project-name)))
           (gits (remove-if-not #'git? locs))
           (non-gits (remove-if #'git? locs)))

      ;; Some "non-gits" are actually gits. Find those and move them.
      (let ((false-non-gits nil))
        (mapc (lambda (loc)
                (when (and (ends-with ".git" (third loc))
                           (search "git" (symbol-name (second loc)) :test #'string-equal))
                  (push loc false-non-gits)))
              non-gits)
        (dolist (loc false-non-gits)
          (push loc gits)
          (setf non-gits (remove loc non-gits :test #'equalp))))
              
      (dolist (d gits)
        (format t "~%git clone ~@[~A~] ~S" clone-args (ensure-ends-with-git (third d))))
      (format t "~%~%Non-git dependencies:")
      (dolist (d non-gits)
        (format t "~%~S" d)))))
    
#|
? (qf::make-clone-commands :rcl)

git clone "https://github.com/sionescu/bordeaux-threads.git"
git clone "https://github.com/melisgl/named-readtables.git"
git clone "https://gitlab.common-lisp.net/alexandria/alexandria.git"
git clone "https://github.com/Shinmera/dissect.git"
git clone "https://github.com/cl-babel/babel.git"
git clone "https://github.com/trivial-garbage/trivial-garbage.git"
git clone "https://github.com/Shinmera/array-utils.git"
git clone "https://github.com/Shinmera/simple-tasks.git"
git clone "https://github.com/trivial-features/trivial-features.git"

Non-git dependencies:
("cffi" :HTTPS "https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz")
("uiop" :HTTPS "https://common-lisp.net/project/asdf/archives/uiop.tar.gz")
("rcl" :HTTPS "https://common-lisp.net/project/rcl/rcl.tar.gz")
NIL
? 

Note that above is not entirely accurate because quicklisp is not.
cffi actually has a github repo at https://github.com/cffi/cffi.git
and uiop is probably best obtained by:

(require "asdf")
(asdf:load-system "uiop")

Although the master repo for uiop is apparently at
https://gitlab.common-lisp.net/asdf/asdf
where it is unfortunately part of the asdf repo.

|#
#|
? (qf::make-clone-commands :rcl "--mirror")

git clone --mirror "https://github.com/sionescu/bordeaux-threads.git"
git clone --mirror "https://github.com/melisgl/named-readtables.git"
git clone --mirror "https://gitlab.common-lisp.net/alexandria/alexandria.git"
git clone --mirror "https://github.com/Shinmera/dissect.git"
git clone --mirror "https://github.com/cl-babel/babel.git"
git clone --mirror "https://github.com/trivial-garbage/trivial-garbage.git"
git clone --mirror "https://github.com/Shinmera/array-utils.git"
git clone --mirror "https://github.com/Shinmera/simple-tasks.git"
git clone --mirror "https://github.com/trivial-features/trivial-features.git"

Non-git dependencies:
("cffi" :HTTPS "https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz")
("uiop" :HTTPS "https://common-lisp.net/project/asdf/archives/uiop.tar.gz")
("rcl" :HTTPS "https://common-lisp.net/project/rcl/rcl.tar.gz")
NIL
? 
|#
