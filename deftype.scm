;; 'define-type'-like functionality for Termite
;;
;; Mutable record created with this are implemented as processes.

;;(define-macro (define-termite-type type id tag . fields)
(define-syntax (define-termite-type body r c)
  (let ((type (cadr body))
        (id (caddr body))
        (tag (cadddr body))
        (fields (cadddr (cdr body))))

  (define (symbol-append . symbols)
    (string->symbol
     (apply
      string-append
      (map symbol->string symbols))))
  
  (define (make-maker type)
    (symbol-append 'make '- type))
  
  (define (make-getter type field)
    (symbol-append type '- field))
  
  (define (make-setter type field)
    (symbol-append type '- field '-set!))
  
  (if (not (eq? id id:))
      (error "id: is mandatory in define-termite-type"))
  
  (let* ((maker (make-maker type))
         (getters (map (lambda (field) 
                         (make-getter type field)) 
                       fields))
         (setters (map (lambda (field) 
                         (make-setter type field)) 
                       fields))
         
         (internal-type (r 'type))
         (internal-maker (make-maker internal-type))
         (internal-getters (map (lambda (field) 
                                  (make-getter internal-type field))
                                fields))
         (internal-setters (map (lambda (field)
                                  (make-setter internal-type field))
                                fields))
         
         (facade-maker (r 'maker))
         (plugin (r '(symbol-append type '-plugin)))
         
         (pid (r 'pid)))
    
    `(,(r 'begin)
       (,(r 'define-record) ,type
         id: ,tag
         constructor: ,facade-maker
         unprintable:
         ,pid)
       
       (,(r 'define-record) ,internal-type
         ,@fields)
       
       (,(r 'define) ,plugin
         (,(r 'make-server-plugin)
          ;; init
          (,(r 'lambda) ,(r 'args)
            (,(r 'apply) ,internal-maker ,(r 'args)))
          ;; call
          (,(r 'lambda) (term state)
            (,(r 'match) term
              ,@((r 'map) ((r 'lambda) ((r 'getter) internal-getter)
                       `(',(r 'getter) (,(r 'values) (,internal-getter state) state)))
                     getters
                     internal-getters)))
          ;; cast
          (,(r 'lambda) (term state)
            (,(r 'match) term
              ,@((r 'map) ((r 'lambda) (setter internal-setter)
                       `((',setter x) (,internal-setter state x) state))
                     setters
                     internal-setters)))
          ;; terminate
          (,(r 'lambda) (reason state)
            (void))))
       
       (,(r 'define) (,maker ,@fields)
         (,facade-maker (server:start ,plugin (list ,@fields) name: ',type)))
       
       ,@((r 'map) (lambda (getter)
                `(,(r 'define) (,getter x)
                   ((,r 'server:call) (,(make-getter type pid) x) 
                                ',getter)))
              getters)

       ,@((r 'map) ((r 'lambda) (setter)
                `(,(r 'define) (,setter x value)
                   (,(r 'server:cast) (,(make-getter type pid) x) 
                                (,(r 'list) ',setter value))))
              setters)))))
