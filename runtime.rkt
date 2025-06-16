#lang racket

(provide (all-defined-out))

;; 结构体定义
(struct class (name super fields methods) #:mutable #:transparent)
(struct object (class fields) #:mutable #:transparent)
(struct method (params body is-private) #:transparent)

;; 全局类环境
(define class-env (make-hash))

(define (lookup-class name)
  (hash-ref class-env name #f))

(define (register-class! name cls)
  (hash-set! class-env name cls))

(define (make-private-key name)
  (string->symbol (format "private:~a" name)))

;; 检查私有字段权限
(define (check-field-access obj field env)
  (define private-key (make-private-key field))
  (define fields (object-fields obj))
  
  (cond
    ;; 如果以私有形式存在且当前不在方法上下文中
    [(and (hash-has-key? fields private-key)
          (not (assoc 'this env)))
     (error (format "Cannot access private field: ~a" field))]
    
    ;; 如果以私有形式存在且用户尝试使用私有前缀访问
    [(and (hash-has-key? fields private-key)
          (regexp-match? #rx"^private:" (symbol->string field)))
     (error (format "Private fields should be accessed by name: ~a" (substring (symbol->string field) 8)))]
    
    ;; 合法访问
    [else #t]))

;; 创建对象实例
(define (instantiate cls args env)
  (define fields (make-hash))
  (define parent (class-super cls))
  
  ;; 继承父类字段
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
    (let ([this-env (list (cons 'this obj))])
      (apply-method constr obj this-env args env)))
  
  obj)

;; 查找方法
(define (find-method cls name)
  (let loop ([current cls])
    (cond
      [(not current) #f]
      [(hash-ref (class-methods current) name #f) => values]
      [else (loop (class-super current))])))

;; 调用方法
(define (dispatch-method obj method-name args env)
  (define cls (object-class obj))
  (define m (find-method cls method-name))
  
  (unless m
    (error (format "Method not found: ~a in class ~a" method-name (class-name cls))))
  
  (when (method-is-private m)
    (error (format "Cannot call private method: ~a" method-name)))
  
  (define this-env (list (cons 'this obj)))
  (apply-method m obj this-env args env))

;; 执行方法
(define (apply-method m obj this-env args caller-env)
  (define param-names (method-params m))
  (unless (= (length param-names) (length args))
    (error "Argument count mismatch"))
  
  ;; 创建参数绑定
  (define arg-env
    (for/list ([name param-names] [arg args])
      (cons name arg)))
  
  ;; 组合环境
  (eval-expr (append this-env arg-env caller-env) (method-body m)))

;; 访问字段
(define (access-field obj field env)
  (check-field-access obj field env)
  (define fields (object-fields obj))
  (define private-key (make-private-key field))
  
  (cond
    [(hash-ref fields field #f) => values]
    [(hash-ref fields private-key #f) => values]
    [else (error (format "Field not found: ~a" field))]))

;; 设置字段
(define (set-field! obj field value env)
  (check-field-access obj field env)
  (define fields (object-fields obj))
  (define private-key (make-private-key field))
  
  (cond
    [(hash-has-key? fields field)
     (hash-set! fields field value)]
    [(hash-has-key? fields private-key)
     (hash-set! fields private-key value)]
    [else (error (format "Field not found: ~a" field))]))

;; 环境管理
(define (make-base-environment)
  (list
   (cons 'true #t)
   (cons 'false #f)
   (cons 'null null)
   (cons 'print (lambda (v) (display v) v))
   (cons 'println (lambda (v) (displayln v) v))
   (cons 'string-append string-append)
   (cons 'number->string number->string)
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '>= >=)
   (cons '= =)))

(define (lookup-var env var)
  (let ([pair (assoc var env)])
    (if pair
        (cdr pair)
        (error (format "Undefined variable: ~a" var)))))

;; 表达式求值
(define (eval-expr env expr)
  (match expr
    ;; 类定义
    [`(class ,name ,super-part ,body ...)
     (let* ([super (match super-part
                    [`(extends ,super-name) (lookup-class super-name)]
                    ['() #f]
                    [_ (error "Invalid superclass specification:" super-part)])]
            [fields (make-hash)]
            [methods (make-hash)])
       
       (for ([expr body])
         (match expr
           [`(field ,field-name ,init-expr)
            (let ([value (eval-expr env init-expr)])
              (hash-set! fields field-name value))]
           
           [`(private ,field-name ,init-expr)
            (let ([private-key (make-private-key field-name)]
                  [value (eval-expr env init-expr)])
              (hash-set! fields private-key value))]
           
           [`(method ,method-name ,params ,body-expr)
            (hash-set! methods method-name (method params body-expr #f))]
           
           [`(private-method ,method-name ,params ,body-expr)
            (hash-set! methods method-name (method params body-expr #t))]
           
           [`(constructor ,params . ,body-exprs)
            (let ([body-expr `(begin ,@body-exprs)])
              (hash-set! methods 'constructor (method params body-expr #f)))]
           
           [_
            (error "Invalid class body element:" expr)]))
       
       (define new-class (class name super fields methods))
       (register-class! name new-class)
       new-class)]
    
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
     (let ([val (eval-expr env value)])
       ;; 将新变量添加到环境
       (cons (cons var val) env))]
    
    ;; 变量引用
    [(? symbol? var) (lookup-var env var)]
    
    ;; 字面量
    [(? number? n) n]
    [(? string? s) s]
    [(? boolean? b) b]
    
    ;; if条件
    [`(if ,cond ,then ,else)
     (if (eval-expr env cond)
         (eval-expr env then)
         (eval-expr env else))]
    
    ;; 块表达式
    [`(begin ,exprs ...)
     (for/fold ([current-env env] [result (void)] #:result result)
               ([e exprs])
       (let ([res (eval-expr current-env e)])
         (values (if (and (list? res) (eq? (car res) 'define)) res current-env)
                 res)))]
    
    ;; 函数应用
    [`(,func . ,args)
     (define f (eval-expr env func))
     (if (procedure? f)
         (apply f (map (curry eval-expr env) args))
         (error (format "Not a procedure: ~a" f)))]
    
    ;; 直接值
    [v v]))

;; 程序求值
(define (eval-program exprs)
  (let loop ([env (make-base-environment)] [exprs exprs])
    (if (null? exprs)
        (void)
        (let* ([result (eval-expr env (car exprs))]
               [new-env (if (and (list? result) (eq? (car (car exprs)) 'define)) 
                           result 
                           env)])
          (loop new-env (cdr exprs))))))