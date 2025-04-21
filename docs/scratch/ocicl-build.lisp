(defpackage :aoforce/docs/scratch/ocicl-build
  (:use :cl))
(in-package :aoforce/docs/scratch/ocicl-build)

(or
 (when (find-package '#:OCICL-RUNTIME)
   (progv (list (find-symbol "*DOWNLOAD*" '#:OCICL-RUNTIME)
                (find-symbol "*VERBOSE*" '#:OCICL-RUNTIME))
       (list t t)
     (funcall (find-symbol "LOAD-SYSTEM" '#:asdf) :cffi-toolchain)))
#+nil
 (when (find-package '#:QUICKLISP)
   (funcall (find-symbol "QUICKLOAD" '#:QUICKLISP) :cffi-toolchain))
 (when (find-package '#:ASDF)
   (funcall (find-symbol "LOAD-SYSTEM" '#:ASDF) :cffi-toolchain))
 (error "Unable to find any system-loading mechanism."))
