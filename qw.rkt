#lang racket

(require racket/match
         racket/set
         racket/dict)

;; =====================
;; 类型系统定义(模块化组织)
;; =====================
(module type-system racket
  (require racket/dict)
  (provide base-type arrow-type type-var type-scheme list-type
           Nat Bool Unit String
           type-var? arrow-type? list-type? type-scheme?
           free-type-variables instantiate substitute-type
           type-to-string
           fresh-tyvar)

  ;; 透明结构定义
  (struct base-type (name) #:transparent)
  (struct arrow-type (dom rng) #:transparent)
  (struct type-var (id) #:transparent)
  (struct type-scheme (vars body) #:transparent)
  (struct list-type (element-type) #:transparent)

  ;; 预定义基础类型常量
  (define Nat (base-type "Nat"))
  (define Bool (base-type "Bool"))
  (define Unit (base-type "Unit"))
  (define String (base-type "String"))

  ;; 模块内部的计数器
  (define type-var-counter 0)

  ;; 生成新的类型变量
  (define (fresh-tyvar)
    (set! type-var-counter (add1 type-var-counter))
    (type-var (string->symbol (format "t~a" type-var-counter))))

  ;; 自由类型变量计算
  (define (free-type-variables type)
    (match type
      [(type-var id) (set type)]
      [(arrow-type dom rng)
       (set-union (free-type-variables dom) (free-type-variables rng))]
      [(list-type elem-type) (free-type-variables elem-type)]
      [_ (set)]))

  ;; 类型方案实例化
  (define (instantiate scheme)
    (match scheme
      [(type-scheme vars body)
       (let ([subst (map (λ (v) (cons v (fresh-tyvar))) vars)])
         (substitute-type body subst))]
      [_ scheme]))

  ;; 类型替换(使用let*避免作用域问题)
  (define (substitute-type type subst)
    (match type
      [(type-var id)
       (or (dict-ref subst type #f) type)]
      [(arrow-type dom rng)
       (arrow-type (substitute-type dom subst)
                   (substitute-type rng subst))]
      [(list-type elem-type)
       (list-type (substitute-type elem-type subst))]
      [(type-scheme vars body)
       (let* ([new-subst (filter (λ (pair) (not (member (car pair) vars))) subst)]
              [new-vars (map (λ (v) (type-var (gensym (type-var-id v)))) vars)]
              [new-var-map (map cons vars new-vars)])
         (type-scheme new-vars 
                      (substitute-type body (append new-var-map new-subst))))]
      [_ type]))

  ;; 类型显示函数
  (define (type-to-string type)
    (match type
      [(base-type name) name]
      [(arrow-type dom rng) 
       (format "(~a → ~a)" (type-to-string dom) (type-to-string rng))]
      [(type-var id) (symbol->string id)]
      [(list-type elem-type)
       (format "[~a]" (type-to-string elem-type))]
      [else (format "~a" type)])))

;; =====================
;; 表达式定义(语义清晰的命名)
;; =====================
(struct var (name) #:transparent)
(struct lambda (param param-type body) #:transparent)
(struct app (rator rand) #:transparent)
(struct num (value) #:transparent)
(struct bool (value) #:transparent)
(struct if-expr (cond then else) #:transparent)
(struct binop (op left right) #:transparent)
(struct nil () #:transparent)  ; 简化nil结构
(struct cons (head tail) #:transparent)
(struct list-expr (elements) #:transparent)
(struct fix (func) #:transparent)
(struct type-lambda (type-var body) #:transparent #:constructor-name make-type-lambda)
(struct type-app (expr type) #:transparent)
(struct let-expr (var expr body) #:transparent)
(struct letpoly-expr (var expr body) #:transparent)
(struct builtin (name handler) #:transparent)  ; 更清晰的命名

;; =====================
;; 类型环境管理(使用字典API)
;; =====================
(define (empty-env) '())
(define (extend-env env var type) (dict-set env var type))
(define (lookup-env env var) (dict-ref env var #f))

;; =====================
;; 类型检查核心(增强错误处理)
;; =====================
(define type-var-counter 0)

(define (fresh-tyvar)
  (set! type-var-counter (add1 type-var-counter))
  (type:type-var (string->symbol (format "t~a" type-var-counter)))) ; 使用带前缀的 type-var

;; 统一类型检查(添加详细错误信息)
(define (unify t1 t2)
  (cond
    [(equal? t1 t2) #t]
    [(type:type-var? t1) ; 使用带前缀的类型判断
     (if (occurs-check t1 t2)
         (error (format "递归类型: ~a 出现在 ~a" t1 t2))
         #t)]
    [(type:type-var? t2) 
     (if (occurs-check t2 t1)
         (error (format "递归类型: ~a 出现在 ~a" t2 t1))
         #t)]
    [(and (type:arrow-type? t1) (type:arrow-type? t2)) ; 使用带前缀的类型判断
     (and (unify (type:arrow-type-dom t1) (type:arrow-type-dom t2)) ; 使用带前缀的访问器
          (unify (type:arrow-type-rng t1) (type:arrow-type-rng t2)))]
    [else (error (format "类型不匹配: ~a 和 ~a" t1 t2))]))

;; 出现检查(防止无限递归)
(define (occurs-check var type)
  (match type
    [(type:arrow-type dom rng)
     (or (occurs-check var dom) (occurs-check var rng))]
    [(type:list-type elem-type) (occurs-check var elem-type)]
    [_ (equal? var type)]))

;; 主类型检查函数(使用with-handlers捕获异常)
(define (type-check expr env)
  (with-handlers ([exn? (λ (e) (error (format "类型检查错误: ~a\n在表达式: ~a" (exn-message e) expr)))])
    (match expr
      [(var name)
       (let ([t (lookup-env env name)])
         (if t (instantiate t)
             (error (format "未绑定变量: ~a" name))))]
      
      [(lambda param param-type body)
       (let* ([new-env (extend-env env param param-type)]
              [body-type (type-check body new-env)])
         (type:arrow-type param-type body-type))] ; 使用带前缀的 arrow-type
      
      [(app rator rand)
       (let* ([rator-type (type-check rator env)]
              [rand-type (type-check rand env)])
         (unless (type:arrow-type? rator-type)
           (error (format "非函数应用: ~a" rator-type)))
         (unless (unify (type:arrow-type-dom rator-type) rand-type)
           (error (format "参数类型不匹配: 预期 ~a, 实际 ~a"
                          (type:arrow-type-dom rator-type) rand-type)))
         (type:arrow-type-rng rator-type))]
      
      [(num _) (type:base-type "Nat")] ; 使用带前缀的 base-type
      
      [(bool _) (type:base-type "Bool")]
      
      [(if-expr cond-expr then-expr else-expr)
       (let* ([cond-type (type-check cond-expr env)]
              [then-type (type-check then-expr env)]
              [else-type (type-check else-expr env)])
         (unless (unify cond-type (type:base-type "Bool"))
           (error "条件表达式必须为布尔类型"))
         (unless (unify then-type else-type)
           (error (format "分支类型不一致: then ~a, else ~a" then-type else-type)))
         then-type)]
      
      [(binop op left right)
       (let* ([left-type (type-check left env)]
              [right-type (type-check right env)])
         (case op
           [(+ - *) 
            (unless (unify left-type (type:base-type "Nat"))
              (error "左操作数必须为自然数"))
            (unless (unify right-type (type:base-type "Nat"))
              (error "右操作数必须为自然数"))
            (type:base-type "Nat")]
           [(< =)
            (unless (unify left-type right-type)
              (error (format "操作数类型不匹配: ~a 和 ~a" left-type right-type)))
            (type:base-type "Bool")]
           [else (error (format "未知操作符: ~a" op))]))]
      
      [(nil) (type:list-type (type:fresh-tyvar))]  ; 简化nil处理
      
      [(cons head tail)
       (let ([head-type (type-check head env)]
             [tail-type (type-check tail env)])
         (unless (type:list-type? tail-type)
           (error "cons的第二个参数必须是列表"))
         (unless (unify head-type (type:list-type-element-type tail-type))
           (error (format "元素类型不匹配: 头 ~a, 尾 ~a" head-type (type:list-type-element-type tail-type))))
         tail-type)]
      
      [(list-expr elements)
       (if (null? elements)
           (type:list-type (type:fresh-tyvar))
           (let* ([first-type (type-check (car elements) env)]
                  [rest-types (map (λ (e) (type-check e env)) (cdr elements))])
             (for ([t rest-types])
               (unless (unify first-type t)
                 (error "列表元素必须具有相同类型")))
             (type:list-type first-type)))]
      
      [(fix func)
       (let ([func-type (type-check func env)])
         (unless (type:arrow-type? func-type)
           (error "fix操作符需要函数类型"))
         (let ([dom (type:arrow-type-dom func-type)]
               [rng (type:arrow-type-rng func-type)])
           (unless (unify dom rng)
             (error (format "函数定义域和值域必须匹配: ~a ≠ ~a" dom rng)))
           dom))]
      
      [(builtin _ _) (type:arrow-type (type:fresh-tyvar) (type:fresh-tyvar))]  ; 内置函数特殊处理
      
      [_ (error (format "不支持的表达式类型: ~a" expr))])))

;; =====================
;; 求值器(安全的内存管理)
;; =====================
(struct closure (param body env) #:transparent)

(define (evaluate expr env)
  (match expr
    [(var name) (dict-ref env name)]
    
    [(lambda param _ body)  ; 忽略参数类型注解
     (closure param body env)]
    
    [(app rator rand)
     (let ([func (evaluate rator env)]
           [arg (evaluate rand env)])
       (match func
         [(closure param body closure-env)
          (evaluate body (dict-set closure-env param arg))]
         [(? procedure? proc) (proc arg)]  ; 处理内置函数
         [_ (error "非函数应用")]))]
    
    [(num n) expr]
    [(bool b) expr]
    
    [(if-expr cond-expr then-expr else-expr)
     (let ([cond-val (evaluate cond-expr env)])
       (if (and (bool? cond-val) (bool-value cond-val))
           (evaluate then-expr env)
           (evaluate else-expr env)))]
    
    [(binop op left right)
     (let ([left-val (evaluate left env)]
           [right-val (evaluate right env)])
       (if (and (num? left-val) (num? right-val))
           (let ([l (num-value left-val)]
                 [r (num-value right-val)])
             (num
              (case op
                ['+ (+ l r)]
                ['- (- l r)]
                ['* (* l r)]
                ['< (if (< l r) 1 0)] ; 简化为数值
                ['= (if (= l r) 1 0)])))
           (error "二元操作数必须为数字")))]
    
    [(nil) (nil)]  ; 简化nil创建
    
    [(cons head tail)
     (cons (evaluate head env) (evaluate tail env))]
    
    [(list-expr elements)
     (list-expr (map (λ (e) (evaluate e env)) elements))]
    
    [(fix func)
     (let ([f (evaluate func env)])
       (match f
         [(closure param body closure-env)
          (letrec ([rec (closure param body (dict-set closure-env param rec))])
            rec)]
         [_ (error "fix需要函数参数")]))]
    
    [(builtin _ handler) handler]  ; 直接返回处理函数
    
    [_ (error (format "无法求值的表达式: ~a" expr))]))

;; =====================
;; 内置函数实现(安全边界检查)
;; =====================
(define (create-builtin-handler name func)
  (builtin name 
           (λ (arg)
             (with-handlers ([exn? (λ (e) (error (format "~a 错误: ~a" name (exn-message e))))])
               (func arg)))))

(define tail-handler
  (create-builtin-handler 'tail
                           (λ (lst)
                             (match lst
                               [(cons _ tail) tail]
                               [_ (error "空列表无尾部")]))))

(define head-handler
  (create-builtin-handler 'head
                           (λ (lst)
                             (match lst
                               [(cons head _) head]
                               [_ (error "空列表无头部")]))))

(define empty?-handler
  (create-builtin-handler 'empty?
                          (λ (lst)
                            (bool (nil? lst)))))

;; =====================
;; 测试框架(模块化设计)
;; =====================
(module test-framework racket
  (require (prefix-in type: (submod ".." type-system))) ; 使用相对路径引用
  (provide run-test-suite)
  
  ;; 接收type-check作为参数
  (define (run-test type-check evaluate expr name env)
    (printf "===== ~a =====\n" name)
    (printf "表达式: ~a\n" expr)
    (with-handlers ([exn? (λ (e) (printf "错误: ~a\n" (exn-message e)))])
      (let* ([type (type-check expr env)]
             [result (evaluate expr env)])
        (printf "类型: ~a\n" (type:type-to-string type))
        (printf "结果: ~a\n\n" result))))
  
  (define (run-test-suite type-check evaluate tests)
    (for ([test tests])
      (apply (curry run-test type-check evaluate) test))))

;; =====================
;; 测试用例(清晰的测试数据)
;; =====================
(define (create-test-env)
  (extend-env 
   (extend-env 
    (extend-env (empty-env) 'tail tail-handler)
    'head head-handler)
   'empty? empty?-handler))

(define test-cases
  (list
   (list ; 阶乘测试
    (let-expr 'fact
              (fix
               (lambda 'f (type:arrow-type (type:base-type "Nat") (type:base-type "Nat"))
                 (lambda 'n (type:base-type "Nat")
                   (if-expr (binop '= (var 'n) (num 0))
                            (num 1)
                            (binop '* (var 'f) (app (var 'f) (binop '- (var 'n) (num 1)))))))
              (app (var 'fact) (num 5)))
    "阶乘函数"
    (create-test-env))
   
   (list ; 多态恒等函数
    (letpoly-expr 'id
                  (make-type-lambda (type:type-var 'a) ; 使用构造函数和带前缀的 type-var
                                    (lambda 'x (type:type-var 'a) (var 'x)))
                  (app (type-app (var 'id) (type:base-type "Nat")) (num 10)))
    "多态恒等函数"
    (create-test-env))
   
   (list ; 列表长度
    (let-expr 'length
              (fix 
               (lambda 'length 
                 (type:arrow-type (type:list-type (type:type-var 'a)) (type:base-type "Nat"))
                 (lambda 'lst (type:list-type (type:type-var 'a))
                   (if-expr (app (var 'empty?) (var 'lst))
                            (num 0)
                            (binop '+ (num 1) 
                                   (app (var 'length) 
                                        (app (var 'tail) (var 'lst)))))))
              (app (var 'length) 
                   (cons (num 1) 
                         (cons (num 2) 
                               (cons (num 3) 
                                     (nil))))))
    "列表长度"
    (create-test-env))))))

;; =====================
;; 主程序入口
;; =====================
(module+ main
  (require (prefix-in type: (submod ".." type-system)) ; 显式引入 type-system
           (prefix-in tf: (submod ".." test-framework))) ; 使用带前缀的测试框架
  (tf:run-test-suite type-check evaluate test-cases))