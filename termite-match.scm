(module termite-match
        (match/action match)


(import chicken scheme)
(use srfi-1 srfi-4 srfi-18 mailbox mailbox-threads tcp)
(use extras posix ports defstruct)
(require-library lookup-table s11n)
(import (prefix lookup-table chicken:))
(import (prefix s11n s11n:))

(define-syntax (match/action x r c)
  (let ((on-success (cadr x))
        (on-fail (caddr x))
        (datum (cadddr x))
        (clauses (cddddr x))
        (%let (r 'let))
        (%tmp (r 'tmp))
        (%succ (r 'succ))
        (%fail (r 'fail))
        (%lambda (r 'lambda)))
    `(,%let ((,%tmp ,datum)
	   (,%succ (,%lambda() ,on-success)) ;; the thunk for success is lifted
	   (,%fail (,%lambda() ,on-fail)))   ;; the thunk for failure is lifted
       ,(compile-pattern-match `(,%succ) `(,%fail) clauses %tmp))))

;;(define-macro (match datum . clauses)
(define-syntax (match x r c)
  (let ((datum (cadr x))
        (clauses (cddr x))
        (tmp (r 'tmp))
	(fail (r 'fail))
        (%lambda (r 'lambda))
        (%let* (r 'let*))
        (%raise (r 'raise))
        (%list (r 'list)))

	`(,%let* ((,tmp ,datum)
			(,fail (,%lambda () 
					 (,%raise 
					   (,%list bad-match: ,tmp)))))
	   ,(compile-pattern-match
		  #f 
		  `(,fail)
		  clauses
		  tmp))))


)
