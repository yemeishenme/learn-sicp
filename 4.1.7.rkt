#lang racket
(require sicp)

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]                                           ; 自求值语句，直接返回 表达式
        [(quoted? exp) (analyze-quoted exp)]                ; ? 加引号的，返回被引内容
        [(variable? exp) (analyze-variable exp)]            ; 变量， 直接在环境中查找变量的值
        [(assignment? exp) (analyze-assignment exp)]        ; 赋值，递规计算
        [(definition? exp) (analyze-definition exp)]        ; 定义，递规计算
        [(if? exp) (analyze-if exp )]                       ; if表达式， 递规
        [(lambda? exp) (analyze-lambda exp)]                ; lambda，转换成一个可应用的过程
        [(begin? exp) (analyze-sequence (begin-actions exp))] ; begin， 求值一系列表达式，按照出现的顺序
        [(cond? exp) (analyze (cond->if exp))]              ; cond 转换成 if 继续求值
        [(application? exp) (analyze-application exp)]       ; 组合式, 求值运算符部分、运算对象部分，再调用 my-apply将参数传递给过程
        [else
         (error "Unknown expression type -- EVAL" exp)]))   ; 符则返回错误


;; 处理自求值表达式，它返回一个忽略环境参数的执行过程,直接返回相应的表达式
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;; 引号表达式:提取出被引用部分
(define (analyze-quoted exp)
  (let [(qval (text-of-quotation exp))]
    (lambda (env) qval)))

;; 查看变量的值，需要环境，因此仍然需要在执行过程去做
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;; 在分析阶段，完成对 assignment-value 表达式的分析
(define (analyze-assignment exp)
  (let [(var (assignment-variable exp))
        (vproc (analyze (assignment-value exp)))]
    (lambda (env) (set-variable-value! var (vproc env) env)
            'ok)))

;; 分析定义
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;; if 提取谓词、推理和替代部分
(define (analyze-if exp)
  (let ((proc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (proc env))
          (cproc env)
          (aproc env)))))

;; lambda 表达式，分析lambda体
(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env) (make-procedure vars bproc env))))

;;分析序列:序列中的每个表达式都被解析，产生出一个可执上过程, 这些过程组合起来形成一个执行过程
;; 该执行过程以一个环境为参数，顺序的调用各个独立的过程
(define (analyze-sequence exps)
  ; 顺序执行 proc1 proc2
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))

  ; 循环 first-proc rest-procs
  ; 
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  ;
  (let ([procs (map analyze exps)])   ; 通过逐一分析, 获取过程表
    (if (null? procs)          ; 空就报错
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs)))) 

;; 分析过程应用
(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

; 执行
(define (execute-application proc args)
  (cond [(primitive-procedure? proc)              ;; 基本过程
         (apply-primitive-procedure proc args)]   ;; 直接应用
        [(compound-procedure? proc)               ;; 组合过程
         ((procedure-body proc)                   ;; 取得函数体 应用于过扩展环境
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc)))]
        [else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION" proc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define (eval exp env)  ; 参数: 表达式 环境
  (cond [(self-evaluating? exp) exp]                                           ; 自求值语句，直接返回 表达式
        [(variable? exp) (lookup-variable-value exp env)]                      ; 变量， 直接在环境中查找变量的值
        [(quoted? exp) (text-of-quotation exp)]                                ; ? 加引号的，返回被引内容
        [(assignment? exp) (eval-assignment exp env)]                          ; 赋值，递规计算
        [(definition? exp) (eval-definition exp env)]                          ; 定义，递规计算
        [(if? exp) (eval-if exp env)]                                          ; if表达式， 递规
        [(lambda? exp) (make-procedure (lambda-parameters exp)                 ; lambda，转换成一个可应用的过程
                                       (lambda-body exp)
                                       env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]                 ; begin， 求值一系列表达式，按照出现的顺序
        [(cond? exp) (eval (cond->if exp) env)]                                ; cond 转换成 if 继续求值
        [(application? exp) (my_apply (eval (operator exp) env)                ; 组合式, 求值运算符部分、运算对象部分，再调用 my-apply将参数传递给过程
                                      (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type -- EVAL" exp)]))                      ; 符则返回错误
|#
          
;; 定义新的 apply
(define (my_apply procedure arguments) ;; 两个参数：过程 过程参数
  (cond [(primitive-procedure? procedure)                                      ; 基本过程？直接调用
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)                                       ; 复合过程
         (eval-sequence                                                        ; 按顺序求值
          (procedure-body procedure)                                           ;   过程体
          (extend-environment                                                  ;   扩展环境
           (procedure-parameters procedure)                                    ;     过程参数
           arguments                                                           ;     参数
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY" procedure)]))


;; 生成实际参数列表 : 以组合式的运算对象数参数，求值各个运算对象，返回这些值的表
;; **** 这里可以使用 map 来求值，下面这样写，是为了表明，可以不用高阶过程来完成这件事儿 
(define (list-of-values exps env)
  (if (no-operands? exps)    ; 没有运算对象？返回空表
      '()
      (cons (eval (first-operand exps) env)   ; 求值第一个运算对象
            (list-of-values (rest-operands exps) env)))) ;; 递规则求值其它运算运对象


;; 条件  在给定环境中求值谓词部分，如果为真则求值推论部分，否则求值替代部分
(define (eval-if exp env)
  (if (true?
       (eval (if-predicate exp) env))   ;; 求值谓词部分
      (eval (if-consequent exp) env)    ;; 为真 求值推论部分
      (eval (if-alternative exp) env))) ;;     求值替代部分

;; 序列, 用于 my-apply 、begin 用于求值过程体里的表达式序列
(define (eval-sequence exps env)
  (cond [(last-exp? exps)
         (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 赋值和定义

;; 变量赋值
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)               ; 找到变量
                       (eval (assignment-value exp) env)       ; 使用 eval找出需要赋的值，
                       env)
  'ok)

;; 变量定义
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)                  ; 找到表达式中的 变量符号
    (eval (definition-value exp) env)                          ; 求值变量的值
    env)                                                       ; 
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.2 表达式的表示


;; 自求值表达式式只有数字和字符串
(define (self-evaluating? exp)
  (cond [(number? exp) true]  ;; 数字
        [(string? exp) true]  ;; 字符串
        [else false]))

;; 变量用符号表示
(define (variable? exp) (symbol? exp))

;; 引号表达式
(define (quoted? exp)
  (tagged-list? exp 'quote))  ;; 如果第一个符号是 'quote

;; 引号表达式的 表达式部分   (text-of-quotation "(quote a)" ) => "a"
(define (text-of-quotation exp) (cadr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 确定某个表的开始否是不是某个给定的符号
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 赋值 (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))

;; 取得 变量
(define (assignment-variable exp) (cadr exp))

;; 取得 值
(define (assignment-value exp) (caddr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
定义的形式：
(define <var> <value>)
或者
(define (<var> <parameter1> ... <parametern>)
  <body>)  =>
语法糖:
(define <var>
  (lambda (<parameter1> ... <parametern>)
    <body>))

|# 

;; 定义？
(define (definition? exp)
  (tagged-list? exp 'define))  ;; 以 define 开头

;; 取得 <var>
(define (definition-variable exp)
  (if (symbol? (cadr exp))      ;; 取第二个符号
      (cadr exp)
      (caadr exp)))

;; 取得 <value>
(define (definition-value exp)
  (if (symbol? (cadr exp)) ;; 如果列表的第二项是符号
      (caddr exp)          ;; 直接取第三项
      ;;; 否则是叻外一种形式的定义， (define  (<var> <parameter1> ... <parametern>) <body>)
      ;;; 构造成 lambda 表达式 返回
      (make-lambda (cdadr exp)   ;; 参数 formal parameters  : (<parameter1> ... <parametern>)
                   (cddr exp)))) ;; body : (<body>) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda paramenters body)
  (cons 'lambda (cons paramenters body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 条件 if
(define (if? exp) (tagged-list? exp 'if))

;; 取得谓词部分
(define (if-predicate exp) (cadr exp))
;; 取得 then
(define (if-consequent exp) (caddr exp))
;; 取得 else
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
;; 构造 if 表达式
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin
(define (begin? exp) (tagged-list? exp 'begin))
;; 取得 (后面的表达式)
(define (begin-actions exp) (cdr exp))
;; 是否是最后一个？
(define (last-exp? seq) (null? (cdr seq)))
;; 取得第一个
(define (first-exp seq) (car seq))
;; 取得科余的
(define (rest-exps seq) (cdr seq))
;; 序列转换为 表达式，如果需要的话，就在在前面加上 begin
(define (sequence->exp seq)
  (cond [(null? seq) seq]       ;; 空的序列，直接返回
        [(last-exp? seq) (first-exp seq)] ;; 最后一个序列，则直接取第一个
        [else (make-begin seq)])) ;; 否则 构造 为 (begin  <seq>)
;; 构造 begin
(define (make-begin seq) (cons 'begin seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 过程： 不符合上述各种表达式类型的任意复合类型
;; car为 运算符, cdr为运算对象的表
;; 过程?
(define (application? exp) (pair? exp))
;; 取得运算符
(define (operator exp) (car exp)) ;; 之前这里之成 (cdr exp) 所以操作符号
;; 取得 运算对象 表
(define (operands exp) (cdr exp))
;; 没有运算对象？
(define (no-operands? ops) (null? ops))
;; 第一个运算对象
(define (first-operand ops) (car ops))
;; 第二...N个运算对象的表
(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 派生表达式
;; cond 可以表示为 if的嵌套
#|
(cond [(> x 0) x]
      [(= x 0) (display 'zero) 0]
      [else (- x)])
=>
(if (> x 0)
    x
    (if (= x 0)
        (begin (display 'zero)
               0)
        (- x))
|#

; cond
(define (cond? exp) (tagged-list? exp 'cond))

;; 条件：动作 表
(define (cond-clauses exp) (cdr exp))

;; 取得 else 部分
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

; 取得谓词部分
(define (cond-predicate clause) (car clause))

; 取得动作部分 表
(define (cond-actions clause) (cdr clause))

; cond->if 将 cond 归约为 if
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

; 展开 条件：动作 表 为 if 的嵌套
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                           ;; 空的，直接返回fasle
      (let ([first (car clauses)]      ;; 取得第一个
            (rest (cdr clauses)))      ;; 取得剩余的
        (if (cond-else-clause? first)  ;; 判是否是 else条件
            (if (null? rest)           ;; 如果 科余的为空
                (sequence->exp (cond-actions first)) ;; 转化为 表达式
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            ;; 不是 else 子句部分 转化为 if 表达式
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.3 求值器的数据结构

;; 谓词检测
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;;; 为了能处理基本过程，我们假定己经有了以下过程
;; (apply-primitive-procedure <proc> <args>) : 将给定过程应用于 <args>里的参数，并返回应用的结果
;; (primitive-procedure? <proc>) : 检测 <proc>是否是一个过程

;;  复合过程由 形式参数， 过程体，环境 通过构造函数做出来
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

;; 是否以 procedure开头
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

;; 取得过程的参数
(define (procedure-parameters p) (cadr p))

;; 取得过程体
(define (procedure-body p) (caddr p))

;; 取得过程的 环境
(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 对环境的操作, 一个环境就是一个框架的序列, 每个框架都是一个约束的表格
;; 其中的约束关联起一些变量和与之对应的值
#|
1. 返回 <var>在 <env>里面的约束值，如果没有发出一个错误信号
(lookup-variable-value <var> <env>) : 
2. 返回一个新环境，这个环境里包含一个新现框架，其中的所有位于表<variables>里的符号约束到<values>
   里对应的元素上, 其外围环境中 <base-env>
(extend-environment <variables> <values> <base-env>)
;; 定义变量
(define-variable! <var> <value> <env>)
;; 设置变量的值
(set-variable-value! <var> <value> <env>)

|#

;; 环境表示为 一个框架的表,一个环境的外围环境就是这个表的cdr
(define (enclosing-environment env) (cdr env))

;; 第一个环境
(define (first-frame env) (car env))

;; 空环境用 '() 表示
(define the-empty-environment '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 环境里的每个框架都是一对表形成的序对：一个是 这一框架中所有变量的表，还有就是约束值的表
#|
(car '(a b c)
     '(1 2 3))  => a=1, b=2, c=3
|#
;; 创建frame 由 表variables 和 表values cons
(define (make-frame variables values)
  (cons variables values))

;; 获取 变量表
(define (frame-variables frame) (car frame))

;; 取得 值表
(define (frame-values frame) (cdr frame))

;; 在框架中增加值
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
用一个新框架扩充一个环境:
我们让框架由一个变量的表和一个值的表组成，并将他们结合到环境上。如果变量的个数与值的个数不匹配，就发出
一个错误信号
|# 
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      ;; 数量不相等的情况
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

#|
在环境中查找变量：
扫描第一个框架里的变量表，找到，则返回对应的值表里面的值
否则，不能在当前框加里面找到变量，就到外围环境变量面寻找
如此继续下去，直到遇到空环境, 发出一个错误信号
|#
;; 在环境中查找变量
(define (lookup-variable-value var env)
  ;; 循环查找环境,以环境作为变量
  (define (env-loop env)
    ;; 扫描变量表,返回对应的值
    (define (scan vars vals)
      (cond [(null? vars)   ;; 变量的列表为空，则在外围环境中继续查找
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))  ; 找到对了变量
             (car vals)]           ; 直接返回对应的值
            [else          ; 否则继续扫描 下一个变量
             (scan (cdr vars) (cdr vals))]))
    ;; 开始
    (if (eq? env the-empty-environment) ;; 空环境，直接发出错误信号
        (error "Unbound variable" var)

        (let ([frame (first-frame env)]) ;; 获得第一个环境
          (scan (frame-variables frame)
                (frame-values frame)))))
  ;;
  ;; 正式调用
  (env-loop env))


;; 修改变量
(define (set-variable-value! var val env)
  ;
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    ;
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  ; 开始循环
  (env-loop env))

;; 定义变量
;; 在第一个框架里面 查找该变量的约束，如果找到就修改其约束，否则就在在第一个框加中加入这个约束
(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    ;;
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    ;;
    (scan (frame-variables frame)
          (frame-values frame))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 基本过程
#|
基本过程对象的具体表示朝着不重要，只要 apply 能够识别它们，并通过过程primitive-procedure?
和 apply-primitive-procedure 去应用它们。
我们选择的方式是， 是将基本过程都表示为以符号primitive开头的表,在其中包含着Lisp系统里实现现这一基本过
程的那个过程
|#

;; 是否是基础过程？
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

;; ????????????????????
(define  (primitive-implementation proc) (cadr proc))

;; setup-environment 将从一个表里取得基本过程的名字和相应的实现过程
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;;; 其他基本过程
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ))

;; 获取基本过程的名称表
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

;; 获取基本过程的对象表
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; 为了应用一个基本过程， 只需要简单的利同基础Lisp系统，将相应的实现过程应用于实际参数
(define (apply-primitive-procedure proc args)
  ;(apply-in-underlying-scheme ;; 这里假设 (define apply-in-underlying-scheme apply)
  (apply
   (primitive-implementation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 提供一个驱动循环, 模拟lisp 的repl

;; 输入提示符
(define input-prompt ";;; M-Eval input:")
;; 输出提示符
(define output-prompt ";;; M-Eval output:")

;; 驱动循环
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) 
  (newline) 
  (display string) 
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

;; 一个特殊的打印过程，避免打印称合过程的环境部分，因为它可能是一个非常长的表
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'commpound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.4 作为程序运行这个求值器
(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; 全局的环境, 环境中包含 'true 'false
(define the-global-environment (setup-environment))

(driver-loop)

