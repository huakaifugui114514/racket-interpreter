#lang racket

(provide (all-defined-out))

(struct class (name super fields methods) #:mutable #:transparent)
(struct object (class fields) #:mutable #:transparent)
(struct method (params body is-private) #:transparent)

(define class-env (make-hash))

(define (lookup-class name)
  (hash-ref class-env name #f))

(define (register-class! name cls)
  (hash-set! class-env name cls))

(define (make-private-key name)
  (string->symbol (format "private:~a" name)))

(define (is-private-field? name)
  (and (symbol? name) (regexp-match? #rx"^private:" (symbol->string name))))

(define (process-class-body body env)
  (define fields (make-hash))
  (define methods (make-hash))
  
  (for ([expr body])
    (match expr
      [`(field ,name ,init) 
       (hash-set! fields name (eval-expr env init))]
      
      [`(private ,name ,init)
       (hash-set! fields (make-private-key name) (eval-expr env init))]
      
      [`(method ,name ,params ,body)
       (hash-set! methods name (method params body #f))]
      
      [`(private-method ,name ,params ,body)
       (hash-set! methods name (method params body #t))]
      
      [`(constructor ,params ,body ...)
       (hash-set! methods 'constructor (method params `(begin ,@body) #f))]
      
      [_ (error "Invalid class body element:" expr)]))
  
  (values fields methods))