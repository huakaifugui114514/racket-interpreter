#lang racket

(require "class.rkt")
(provide (all-defined-out))

(define (instantiate cls args env)
  (define fields (make-hash))
  (define parent (class-super cls))
  
  ; 初始化父类
  (when parent
    (let ([parent-obj (instantiate parent args env)])
      (for ([(k v) (in-hash (object-fields parent-obj))])
        (hash-set! fields k v))))
  
  ; 初始化当前类字段
  (for ([(k v) (in-hash (class-fields cls))])
    (hash-set! fields k v))
  
  (define obj (object cls fields))
  
  ; 调用构造函数
  (define constr (hash-ref (class-methods cls) 'constructor #f))
  (when constr
    (let ([this-env `((this . ,obj) (super . ,parent))])
      (apply-method constr obj this-env args env)))
  
  obj)

(define (find-method cls name)
  (let loop ([current cls])
    (cond
      [(not current) #f]
      [(hash-ref (class-methods current) name #f) => values]
      [else (loop (class-super current))])))