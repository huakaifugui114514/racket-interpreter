OOLang - 面向对象语言解释器

概述

OOLang 是一个基于 Racket 实现的面向对象语言解释器，支持类与对象的创建、继承关系、方法动态分派及封装等核心特性。该项目为复旦大学 PPOL 课程的 Lab 作业，完整实现了面向对象语言的四大特性：封装、继承、多态和抽象。

功能特性

核心功能
类与对象：

类定义与对象实例化

构造函数支持

字段初始化

继承：

单继承机制

方法重写

父类构造函数调用 (super)

封装：

私有字段 (private)

私有方法 (private-method)

访问控制检查

多态：

动态方法分派

运行时类型识别

高级功能
静态方法支持

类变量（静态字段）

严格错误处理机制

交互式 REPL 环境

语法规范

;; 类定义
(class <ClassName> (extends <ParentClass>)
  (field <field-name> <init-value>)      ; 公共字段
  (private <private-field-name> <init-value>) ; 私有字段
  
  (constructor (<params>)                ; 构造函数
    (super <args>)                       ; 调用父类构造函数
    <body>)
  
  (method <method-name> (<params>)       ; 公共方法
    <method-body>)
  
  (private-method <method-name> (<params>) ; 私有方法
    <method-body>))

;; 对象操作
(new <ClassName> <args>)                 ; 实例化对象
(send <object> <method-name> <args>)     ; 方法调用
(get <object> <field>)                   ; 字段访问
(set <object> <field> <value>)           ; 字段设置

;; 变量定义
(define <var> <value>)

;; 内置函数
(print <value>)                          ; 打印值
(println <value>)                        ; 打印值并换行

项目结构

├── main.rkt             # 主解释器入口

├── runtime.rkt          # 运行时逻辑
├── parser.rkt           # 语法解析器
├── run.sh               # 一键运行脚本
├── tests/               # 测试用例
├── inheritance.oo   # 继承测试

├── encapsulation.oo # 封装测试

└── polymorphism.oo  # 多态测试

└── README.md            # 项目文档

运行指南

环境要求
操作系统：Ubuntu 22.04（推荐）

Racket：v8.0+

安装依赖

sudo apt update
sudo apt install racket

使用说明
运行测试用例

运行所有测试

./run.sh test

运行单个测试

./run.sh tests/inheritance.oo

运行自定义程序

./run.sh path/to/your_program.oo

启动交互式控制台

./run.sh

示例程序

;; demo.oo
(class Animal ()
  (field name "Unknown")
  
  (constructor (n)
    (set this name n))
  
  (method speak ()
    (print (string-append (get this name) " makes a sound"))))

(class Dog (extends Animal)
  (method speak ()
    (print (string-append (get this name) " says: Woof!"))))

(define animal (new Animal "Generic Animal"))
(send animal speak) ; 输出: Generic Animal makes a sound

(define dog (new Dog "Buddy"))
(send dog speak)    ; 输出: Buddy says: Woof!

测试用例
继承测试 (tests/inheritance.oo)

(class Animal ()
  (field name "Unknown")
  
  (constructor (n)
    (set this name n))
  
  (method speak () 
    (print "Animal sound")))

(class Dog (extends Animal)
  (method speak () 
    (print (string-append (get this name) " says: Woof!"))))

(define a (new Animal "Generic Animal"))
(send a speak)

(define d (new Dog "Buddy"))
(send d speak)

封装测试 (tests/encapsulation.oo)

(class BankAccount ()
  (private balance 0)
  
  (method deposit (amount)
    (set this balance (+ (get this balance) amount)))
  
  (method withdraw (amount)
    (if (>= (get this balance) amount)
        (set this balance (- (get this balance) amount))
        "Insufficient funds"))
  
  (method get-balance () 
    (get this balance)))

(define acc (new BankAccount))
(send acc deposit 100)
(send acc withdraw 30)
(send acc get-balance)

;; 以下操作应该报错
(get acc balance)

(set acc balance 0)

多态测试 (tests/polymorphism.oo)

(class Shape ()
  (method area () 0))

(class Rectangle (extends Shape)
  (field width 0)
  (field height 0)
  
  (constructor (w h)
    (set this width w)
    (set this height h))
  
  (method area () 
    (* (get this width) (get this height))))

(class Circle (extends Shape)
  (field radius 0)
  
  (constructor (r)
    (set this radius r))
  
  (method area ()
    (* 3.14159 (get this radius) (get this radius)))

(define rect (new Rectangle 5 10))
(send rect area) ; 返回 50

(define circle (new Circle 7))
(send circle area) ; 返回 153.93791

设计实现

核心数据结构

;; 类结构体
(struct class (name super fields methods) #:mutable #:transparent)

;; 对象结构体
(struct object (class fields) #:mutable #:transparent)

;; 方法结构体
(struct method (params body is-private) #:transparent)

关键算法
方法查找算法（支持继承链）

(define (find-method cls name)
  (let loop ([current cls])
    (cond
      [(not current) #f]
      [(hash-ref (class-methods current) name #f) => values]
      [else (loop (class-super current))])))

动态方法分派

(define (dispatch-method obj method-name args env)
  (define cls (object-class obj))
  (define m (find-method cls method-name))
  ;; 应用方法并绑定 this 环境
  (apply-method m obj this-env args env))

对象实例化（支持继承链）

(define (instantiate cls args env)
  ;; 递归初始化父类
  (when parent
    (let ([parent-obj (instantiate parent args env)])
      (merge-fields parent-obj)))
  ;; 初始化当前类
  (for ([(k v) (in-hash (class-fields cls))])
    (hash-set! fields k v))
  ;; 调用构造函数
  (when constructor (apply-constructor ...)))

错误处理

;; 字段访问控制
(define (access-field obj field env)
  (cond
    [(hash-has-key? fields private-key)
     (error "Cannot access private field:" field)]
    ...))

;; 方法调用检查
(define (dispatch-method ...)
  (when (method-is-private m)
    (error "Cannot call private method:" method-name)))

运行示例

$ ./run.sh tests/inheritance.oo
Generic Animal makes a sound
Buddy says: Woof!

$ ./run.sh
OOLang Interactive Console =

OOLang> (class Counter () (private count 0) (method increment () (set this count (+ (get this count) 1))))
OOLang> (define c (new Counter))
OOLang> (send c increment)
OOLang> (send c increment)
OOLang> (get c count) ; 应该报错
Error: Cannot access private field: count

贡献者
姓名：Your Name

学号：Your Student ID

邮箱：your.email@example.com

许可证

本项目采用 MIT 许可证。详情请参阅 LICENSE 文件。