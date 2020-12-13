;;; patches.lisp
;;; patches to quicklisp to support quickfork
;;; 08-Mar-2018 SVS

;;; Ensure that every time quickload is called, it will keep track of every
;;;  asdf system that it loads and/or compiles.

(in-package :quicklisp)
(export '(*compiled-systems*
          *loaded-systems*
          *installed-systems*)
        :quicklisp)

(defparameter *compiled-systems* nil "Systems compiled by quickload")
(defparameter *loaded-systems* nil "Systems loaded by quickload")
(defparameter *installed-systems* nil "Releases installed (downloaded or reexpanded from ~
a downloaded archive) by Quicklisp")

(defmethod quickload :before (systems &key verbose prompt explain
                                         &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (setf *compiled-systems* nil
        *loaded-systems* nil
        *installed-systems* nil))

(defmethod quickload :after (systems &key verbose prompt explain
                                         &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (format t "~%Systems compiled by QL: ")
  (if *compiled-systems*
      (dolist (system *compiled-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%Systems loaded by QL: ")
  (if *loaded-systems*
      (dolist (system *loaded-systems*)
        (format t "~% ~S" system))
      (format t "(none)"))
  (format t "~%Systems installed by QL: ")
  (if *installed-systems*
      (dolist (release *installed-systems*)
        (format t "~% ~S" (ql-dist::project-name release)))
      (format t "(none)"))
  (format t "~%Inspect ql:*compiled-systems*, ql:*loaded-systems*, and ~
ql:*installed-systems* for more info."))

(defmethod asdf:perform :around ((op asdf::basic-compile-op) (file asdf:CL-SOURCE-FILE))
  (let* ((system (asdf:component-system file))
         (loc (where-is-system system))
         (syslist nil))
    (when loc
      (setf syslist (list (asdf:component-name system)
                          loc))
      (unless (find (asdf:component-name system) *compiled-systems*
                    :test 'equalp
                    :key 'car)
        (setf *compiled-systems* (append *compiled-systems* (list syslist))))))
  (call-next-method))

(defmethod asdf:perform :around ((op asdf::basic-load-op) (c asdf:system))
  (let ((loc (where-is-system c))
        (syslist nil))
    (when loc
      (setf syslist (list (asdf:component-name c)
                          loc))
      (setf *loaded-systems* (append *loaded-systems* (list syslist))))
  (call-next-method)))

(defmethod ql-dist::install :after ((release ql-dist::release))
  (setf *installed-systems* (append *installed-systems* (list release))))
