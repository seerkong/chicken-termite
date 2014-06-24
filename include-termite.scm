;; File: "termite#.scm"
;; Copyright (C) 2005-2008 Guillaume Germain

;; This is the declaration file for the Termite system
(import chicken scheme)
(use srfi-1 srfi-4 srfi-18 mailbox mailbox-threads tcp)
(use extras posix ports defstruct)
(require-library lookup-table s11n)
(import (prefix lookup-table chicken:))
(import (prefix s11n s11n:))

(define-syntax (compile-time-load form r c)
  (let ((filename (cadr form)))
  `(,(r 'load) ,filename)))

;implement or rename gambit functions to match their
;chicken equivalents
(include "gambit-glue.scm")

;; make it available at run-time
(import-for-syntax chicken scheme)
(begin-for-syntax 
(include "match-support.scm")
(include "match.scm"))

;; ----------------------------------------------------------------------------
;; Macros 

(include "match.scm")
(include "recv.scm")
(include "deftype.scm")
(include "uuid.scm")

(include "termite.scm")

