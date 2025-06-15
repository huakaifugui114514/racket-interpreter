#lang racket

(require "class.rkt"
         "inheritance.rkt")
(provide (all-defined-out))

(define (dispatch-method obj method-name args env)
  (define cls (object-class obj))
  (define m (find-method cls method-name))
  
  (unless m
    (error "Method not found:" method-name))
  
  (when (method-is-private m)
    (error "Cannot call private method:" method-name))
  
  (define this-env `((this . ,obj)))
  (apply-method m obj this-env args env))

(define (apply-method m obj env args caller-env)
  (define param-names (method-params m))
  (unless (= (length param-names) (length args))
    (error "Argument count mismatch"))
  
  ; 创建参数绑定
  (define arg-env
    (for/list ([name param-names] [arg args])
      (cons name arg)))
  
  ; 组合环境：this绑定 + 参数绑定 + 外部环境
  (eval-expr (append arg-env env caller-env) (method-body m) caller-env))

(define (access-field obj field env)
  (define fields (object-fields obj))
  (define private-key (make-private-key field))
  
  (cond
    [(hash-has-key? fields private-key)
     (error "Cannot access private field:" field)]
    [(hash-ref fields field #f) => values]
    [else (error "Field not found:" field)]))

(define (set-field! obj field value env)
  (define fields (object-fields obj))
  (define private-key (make-private-key field))
  
  (cond
    [(hash-has-key? fields private-key)
     (error "Cannot set private field:" field)]
    [(hash-has-key? fields field)
     (hash-set! fields field value)]
    [else (error "Field not found:" field)]))