#lang racket

(require "class.rkt"
         "inheritance.rkt"
         "dispatch.rkt")
(provide (all-defined-out))

(define (make-base-environment)
  `((true . #t)
    (false . #f)
    (null . null)))

(define (lookup-var env var)
  (match (assoc var env)
    [(cons _ value) value]
    [#f (error "Undefined variable:" var)]))

(define (extend-env env bindings)
  (append bindings env))

(define (eval-expr env expr)
  (match expr
    ; 类定义
    [`(class ,name (extends ,super) ,body ...)
     (define parent (lookup-class super))
     (define-values (fields methods) (process-class-body body env))
     (define new-class (class name parent fields methods))
     (register-class! name new-class)
     new-class]
    
    ; 对象实例化
    [`(new ,class-name ,args ...)
     (define cls (lookup-class class-name))
     (unless cls (error "Class not defined:" class-name))
     (define arg-values (map (curry eval-expr env) args))
     (instantiate cls arg-values env)]
    
    ; 方法调用
    [`(send ,obj-expr ,method-name ,args ...)
     (define obj (eval-expr env obj-expr))
     (define arg-values (map (curry eval-expr env) args))
     (dispatch-method obj method-name arg-values env)]
    
    ; 字段访问
    [`(get ,obj-expr ,field)
     (define obj (eval-expr env obj-expr))
     (access-field obj field env)]
    
    ; 字段设置
    [`(set ,obj-expr ,field ,value-expr)
     (define obj (eval-expr env obj-expr))
     (define value (eval-expr env value-expr))
     (set-field! obj field value env)]
    
    ; 变量引用
    [(? symbol? var) (lookup-var env var)]
    
    ; 字面量
    [(? number? n) n]
    [(? string? s) s]
    [(? boolean? b) b]
    
    ; 块表达式
    [`(begin ,exprs ...)
     (for/last ([e exprs]) (eval-expr env e))]
    
    ; 函数应用
    [`(,func . ,args)
     (define f (eval-expr env func))
     (if (procedure? f)
         (apply f (map (curry eval-expr env) args))
         (error "Not a procedure:" f))]
    
    ; 直接值
    [v v]))

(define (eval-program exprs)
  (define env (make-base-environment))
  (for/last ([expr exprs])
    (eval-expr env expr)))