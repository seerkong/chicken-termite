(use srfi-4)

(define (close-port p)
   (cond ((input-port? p) (close-input-port p))
         ((output-port? p) (close-output-port p))
         (else (error "not a port"))))

(define (read-subu8vector vector start end
         #!optional (port (current-input-port)) (needs 0))
   (read-u8vector! (if (> needs 0) needs (- end start)) vector port start))

(define (write-subu8vector vector start end
           #!optional (port (current-output-port)))
   (write-u8vector vector port start end))

(define (extract-bit-field width start field)
   (arithmetic-shift 
      (bitwise-and field
         (arithmetic-shift (- (arithmetic-shift 1 width) 1) start))
      (* -1 start)))

(define (make-random-source) #t)
(define (random-source-make-integers dontcare)
    random)
(define (random-source-randomize! dontcare)
   (randomize))

(define (make-table #!key size init weak-keys weak-values test hash min-load max-load)
  (apply chicken:make-dict (cons (or test equal?) (if size (list size) '()))))


(define table->list chicken:dict->alist)
(define table-set! chicken:dict-set!)
(define table-ref chicken:dict-ref)
(define table-search chicken:dict-search)
(define table-length chicken:dict-count)
(define table-for-each chicken:dict-for-each)


(define (u8vector->object obj #!optional deserializer)
   (let ((tmp (with-output-to-string (write-u8vector obj))))
      (with-input-from-string tmp (s11n:deserialize
                             (current-input-port)
                             deserializer))))

(define (object->u8vector obj #!optional serializer)
   (let* ((tmp (with-output-to-string
                 (s11n:serialize (current-output-port) serializer)))
         (len (length tmp)))
      (with-input-from-string tmp (read-u8vector! len))) )

(define (force-output . args) #t)
