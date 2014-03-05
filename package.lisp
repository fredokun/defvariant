
(defpackage #:defvariant
  (:nicknames #:variant)
  (:use #:cl)
  (:export
   #:defvariant
   #:match-error
   #:match-error-message
   #:match-warning
   #:match-warning-message))

(in-package #:defvariant)

;; this is needed to disable to
;; inline example (only for top-level evaluations).
(defparameter *example-enabled* nil)

