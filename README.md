# OOLang - 面向对象语言解释器

## 功能说明
1. **类与对象**：
   - 类定义与对象实例化
   - 构造函数支持
   - 字段初始化
   
2. **继承**：
   - 单继承机制
   - 方法重写
   - 父类构造函数调用
   
3. **封装**：
   - 私有字段 (`private`)
   - 私有方法 (`private-method`)
   - 访问控制检查
   
4. **多态**：
   - 动态方法分派
   - 运行时类型识别
   
5. **其他特性**：
   - 基本数据类型支持（数值、字符串、布尔值）
   - 变量绑定 (`define`)
   - 代码块 (`begin`)

## 语法规范
```racket
; 类定义
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

; 对象操作
(new <ClassName> <args>)                 ; 实例化对象
(send <object> <method-name> <args>)     ; 方法调用
(get <object> <field>)                  ; 字段访问
(set <object> <field> <value>)          ; 字段设置

; 变量定义
(define <var> <value>)
```

## 运行指南

### 环境要求
- Racket 8.0+
- 操作系统：Ubuntu 24.04

### 安装步骤
```bash
sudo apt update
sudo apt install racket
```

### 运行程序
```bash
# 运行单个文件
racket oolang.rkt tests/inheritance.oo

# 运行所有测试
find tests -name "*.oo" | xargs -I {} racket oolang.rkt {}
```

### 依赖项
无额外依赖

## 测试用例说明
1. `inheritance.oo`：测试类继承和方法重写
2. `encapsulation.oo`：测试私有字段访问控制
3. `polymorphism.oo`：测试动态方法分派和多态

## 设计亮点
1. **模块化架构**：清晰分离语法解析、运行时、继承处理等模块
2. **完整的封装支持**：严格实施私有字段/方法访问控制
3. **动态分派优化**：高效的方法查找算法支持继承链
4. **构造函数链**：支持`super`调用父类构造函数
5. **错误处理**：详细的错误信息帮助定位问题