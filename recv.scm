;; All hail the RECV form
(define-syntax (recv x r c)
  (let ((clauses (cdr x))
        (%msg  (r 'msg))   ;; the current mailbox message
        (%loop (r 'loop)) ;; the mailbox seeking loop
        (%with-exception-handler (r 'with-exception-handler))
        (%lambda (r 'lambda))
        (%mailbox-timeout-exception?
           (r 'mailbox-timeout-exception?))
        (%begin (r 'begin))
        (%thread-mailbox-rewind (r 'thread-mailbox-rewind))
        (%thread-mailbox-extract-and-rewind
           (r 'thread-mailbox-extract-and-rewind))
        (%thread-mailbox-next (r 'thread-mailbox-next))
        (%handle-exception-message (r 'handle-exception-message))
        (%match/action (r 'match/action))
        (%raise (r 'raise))
        (%event (r 'event))
        (%termite-exception? (r 'termite-exception?))
        (%if (r 'if))
        (%let (r 'let)))
    ;; check the last clause to see if it's a timeout
    (let ((sesualc (reverse clauses)))
      (if (and (pair? (car sesualc))
               (eq? (caar sesualc) 'after))

          (let ((clauses (reverse (cdr sesualc)))
                ;; the code to compute the timeout
                (init (cadar sesualc))
                ;; the variable holding the timeout
                (timeout (r 'timeout))
                ;; the code to be executed on a timeout
                (on-timeout (cddar sesualc))
                ;; the timeout exception-handler to the whole match
                (%e (r 'e)))

            ;; RECV code when there is a timeout
            `(,%let ((,timeout ,init))
               (,%with-exception-handler
                (,%lambda (,%e)
                  (,%if (,%mailbox-timeout-exception? ,%e)
                      (,%begin
                        (,%thread-mailbox-rewind)
                        ,@on-timeout)
                      (,%raise ,%e)))
                (,%lambda ()
                  (,%let ,%loop ((,%msg (,%thread-mailbox-next ,timeout)))
                       (,%match/action
                        (,%thread-mailbox-extract-and-rewind)
                        (,%loop 
                         (,%thread-mailbox-next ,timeout))
                        ,%msg
                        ;; extra clause to handle system events
                        (,%event 
                         (where (,%termite-exception? ,%event))
                         (,%handle-exception-message ,%event))
                        ;; the user clauses
                        ,@clauses))))))

          ;; RECV code when there is no timeout
          `(,%let ,%loop ((,%msg (,%thread-mailbox-next)))
                (,%match/action
                 (,%thread-mailbox-extract-and-rewind)
                 (,%loop
                  (,%thread-mailbox-next))
                 ,%msg
                 ;; extra clause to handle system events
		 (,%event 
		  (where (,%termite-exception? ,%event))
		  (,%handle-exception-message ,%event))
                 ;; the user clauses
                 ,@clauses))))))

