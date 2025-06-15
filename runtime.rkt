#lang racket

(provide (all-defined-out))

;; ===== 结构体定义 =====
(struct class (name super fields methods) #:mutable #:transparent)
(struct object (class fields) #:mutable #:transparent)
(struct method (params body is-private) #:transparent)

;; ===== 全局环境 =====
(define class-env (make-hash))

(define (lookup-class name)
  (hash-ref class-env name #f))

(define (register-class! name cls)
  (hash-set! class-env name cls))

(define (make-private-key name)
  (string->symbol (format "private:~a" name)))

(define (is-private-field? name)
  (and (symbol? name) (regexp-match? #rx"^private:" (symbol->string name))))

;; ===== 继承处理 =====
(define (instantiate cls args env)
  (define fields (make-hash))
  (define parent (class-super cls))
  
  ;; 初始化父类
  (when parent
    (let ([parent-obj (instantiate parent args env)])
      (for ([(k v) (in-hash (object-fields parent-obj))])
        (hash-set! fields k v))))
  
  ;; 初始化当前类字段
  (for ([(k v) (in-hash (class-fields cls))])
    (hash-set! fields k v))
  
  (define obj (object cls fields))
  
  ;; 调用构造函数
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

;; ===== 方法分派 =====
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
  
  ;; 创建参数绑定
  (define arg-env
    (for/list ([name param-names] [arg args])
      (cons name arg)))
  
  ;; 组合环境
  (eval-expr (append arg-env env caller-env) (method-body m)))

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

;; ===== 环境管理 =====
(define (make-base-environment)
  `((true . #t)
    (false . #f)
    (null . null)
    (print . ,(lambda (v) (display v) (newline) v))
    (println . ,(lambda (v) (displayln v) v))))

(define (lookup-var env var)
  (match (assoc var env)
    [(cons _ value) value]
    [#f (error "Undefined variable:" var)]))

(define (extend-env env bindings)
  (append bindings env))

(define (eval-expr env expr)
  (match expr
    ;; 类定义
    [`(class ,name (extends ,super) ,body ...)
     (define parent (lookup-class super))
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
     
     (define new-class (class name parent fields methods))
     (register-class! name new-class)
     new-class]
    
    ;; 对象实例化
    [`(new ,class-name ,args ...)
     (define cls (lookup-class class-name))
     (unless cls (error "Class not defined:" class-name))
     (define arg-values (map (curry eval-expr env) args))
     (instantiate cls arg-values env)]
    
    ;; 方法调用
    [`(send ,obj-expr ,method-name ,args ...)
     (define obj (eval-expr env obj-expr))
     (define arg-values (map (curry eval-expr env) args))
     (dispatch-method obj method-name arg-values env)]
    
    ;; 字段访问
    [`(get ,obj-expr ,field)
     (define obj (eval-expr env obj-expr))
     (access-field obj field env)]
    
    ;; 字段设置
    [`(set ,obj-expr ,field ,value-expr)
     (define obj (eval-expr env obj-expr))
     (define value (eval-expr env value-expr))
     (set-field! obj field value env)]
    
    ;; 变量定义
    [`(define ,var ,value)
     (define val (eval-expr env value))
     (cons (cons var val) env)]
    
    ;; 变量引用
    [(? symbol? var) (lookup-var env var)]
    
    ;; 字面量
    [(? number? n) n]
    [(? string? s) s]
    [(? boolean? b) b]
    
    ;; 块表达式
    [`(begin ,exprs ...)
     (for/fold ([result null] [current-env env])
               ([e exprs])
       (values (eval-expr current-env e) current-env))]
    
    ;; 函数应用
    [`(,func . ,args)
     (define f (eval-expr env func))
     (if (procedure? f)
         (apply f (map (curry eval-expr env) args))
         (error "Not a procedure:" f))]
    
    ;; 直接值
    [v v]))

(define (eval-program exprs)
  (define env (make-base-environment))
  (for/fold ([result null] [current-env env])
            ([expr exprs])
    (values (eval-expr current-env expr) current-env)))