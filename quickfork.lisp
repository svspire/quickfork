;;; quickfork.lisp
;;; 27-Feb-2018 SVS
;;; Quicklisp for teams

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
  (format t "~%Compiled systems: ")
  (if cl-user::*compiled-systems*
      (dolist (system cl-user::*compiled-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%Loaded systems: ")
  (if cl-user::*loaded-systems*
      (dolist (system cl-user::*loaded-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%QL Installed systems: ")
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
