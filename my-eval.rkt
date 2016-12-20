#lang racket
(require sicp)

;; 判断是否以 tag开头
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


#|
;;
(define (my-eval exp env)
  (cond
    ; 自求值表达式
    [(self-evaluating? exp) exp]
    ; 变量
    [(variable? exp) (lookup-variable-value exp env)]
    ; 引号
    [(quoted? exp) (text-of-quotation exp)]
    ; 赋值
    [(assignment? exp) (eval-assignment exp env)]
    ; 定义
    [(definition? exp) (eval-definition exp env)]
    ; if
    [(if? exp) (eval-if exp env)]
    ; lambda
    [(lambda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body exp)
                     env)]
    ; begin
    [(begin? exp) (eval-sequence (begin-actions exp) env)]
    ; cond
    [(cond? exp) (my-eval (cond->if exp) env)]
    ; let 后增加的情况
    [(let? exp) (my-eval (let->lambda exp) env)]
    ; 函数
    [(application? exp)
     (my-apply (my-eval (operator exp) env)
               (list-of-values (operands exp) env))]
    ; 其它情况，报错
    [else
     (error "Unknown expression type -- EVAL" exp)]))
;|#

(define (my-apply procedure arguments)
  (cond [(primitive-procedure? procedure)    ;; 基本过程
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)     ;; 组合过程
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY" procedure)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 过程参数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 条件
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;序列
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (my-eval (first-exp exps) env)]
        [else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 赋值和定义
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.2 表达式的表示

;; 自求值表达式
(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else
         false]))

;; 变量：用符号表示
(define (variable? exp) (symbol? exp))
;; 引号表达式 (quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; 赋值的形式是 (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

#|
定义 (define <var> <value>)
(define (<var> <parameter1> ... <parametern>)
   <body>)
=>
(define <var>
  (lambda (<parameter1> ... <parametern>)
    <body>))
;|#
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

#|
lambda表达式
(lambda (<parameter1> .. <parametern>>)
  <body>)
|#
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

#|
if :
(if <predicate>
    <consequent>
    <alternative>)
|#
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

#|
begin
(begin <exp1>
       ...
       <expn>)
|#
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else
         (make-begin seq)]))
(define (make-begin seq) (cons 'begin seq))

#|
过程：不属于上述各种表达式类型的任意*复合*表达式。 car 是运算符，cdr 是运算对象的表
|#
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
-
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

#|
派生表达式
(cond [(> x 0) x]
      [(= x 0) (display 'zero) 0]
      [else (- x)])
=>
(if (> x 0)
    x
    (if (= x 0)
        (begin (display 'zero)
               0)
        (- x)))
|#
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let [(first (car clauses))
            (rest  (cdr clauses))]
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
#|
练习 4.6 派生表达式
(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)
=>
((lambda (<var1>
          ...
          <varn>)
   <body>)
 <exp1>
 ...
 <expn>)
|#
(define (let? exp) (tagged-list? exp 'let))

; 取参数表: ((<var1> <exp1>)
;           ...
;          (<varn> <expn>))
; 取所有的变量的表
(define (let-vars exp)
  (map (lambda (x) (car x))
       (cadr exp)))
; 取所有的赋值的表
(define (let-vals exp)
  (map (lambda (x) (cadr x))
       (cadr exp)))
(define (let-body exp)
  (cddr exp))

; 转换为lambda 表达式的形式
(define (let->lambda exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-vals exp)))



;;....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4.1.3 求值器的数据结构
;; 谓词检测
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 过程的表示
;; 假设己有如下基本过程
#|
(apply-primitive-procedure proc args)
(primitive-procedure? proc)
;|#

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 对环境的操作
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
#|
框架由一个变画显示的表和一个值的表组成
(cons (<var1> ... <varn>)
      (<val1> ... <valn>))
|#
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "too few arguments suppled" vars vals))))
;; 在框架中查找变量
(define (lookup-variable-value var env)
  ; 定义函数
  (define (env-loop env)
    ;; 定义 scan
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (car vals)]
            [else
             (scan (cdr vars) (cdr vals))]))
    ;; 调用 scan
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  ; 调用
  (env-loop env))

;; 在框架中设置变量的值
(define (set-variable-value! var val env)
  ; 定义函数
  (define (env-loop env)
    ;; 定义 scan
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    ;; 调用 scan
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  ; 调用
  (env-loop env))
;; 定义一个变量:在第一个框架里寻找该变量的约束，七到则修改；如果不存在，就在第一个框架中增加该约束
(define (define-variable! var val env)
  (let [[frame (first-frame env)]]
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
基本过程的具体形式不重要，只要apply能识别它们，并能通过过程primitive-procedure去应用它们
;|# 
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-impletation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 作为程序运行这个求值器
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    ;; 定义变量表示直/假
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    ;; 返回扩展之后的环境
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  ;(apply-in-underlying-scheme
  (apply
   (primitive-impletation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
提供一个驱动循环,来运行这个求值器
;|#
(define input-prompt  ";;; M-Eval  input<")
(define output-prompt ";;; M-Eval output>")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (begin
             (newline)
             (display "您输入的是：")
             (user-print input)
             (newline)
             (my-eval input the-global-environment))))
          
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))
;; 为了避免打印混合过程的环境部分（可能是一个非常长的表）
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                    (procedure-parameters object)
                    (procedure-body object)
                    '<procedure-env>))
      (display object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type-process
  (list
   (list self-evaluating? (lambda (exp env) exp))
   (list variable?        lookup-variable-value)
   (list quoted?          (lambda (exp env) (text-of-quotation exp)))
   (list assignment?      eval-assignment)
   (list definition?      eval-definition)
   (list if?              eval-if)
   (list lambda?          (lambda (exp env)
                            (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env)))
   (list begin?           (lambda (exp env)
                            (eval-sequence (begin-actions exp) env)))

   ; cond
   (list cond?            (lambda (exp env) (my-eval (cond->if exp) env)))
   (list let?             (lambda (exp env) (my-eval (let->lambda exp) env)))
   (list application?     (lambda (exp env)
                            (my-apply
                             (my-eval (operator exp) env)
                             (list-of-values (operands exp) env))))
   ))

;   [else
;    (error "Unknown expression type -- EVAL" exp)]))
(define (my-eval exp env)
  (define (loop a-list)
    (if (null? a-list)
        (error "Unknown expression type -- EVAL" exp)
        (let ((pred? (caar a-list))
              (process (cadar a-list)))
          (if (pred? exp)
              (process exp env)
              (loop (cdr a-list))))))
  (loop type-process))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 启动求值器
#|
(define the-global-environment (setup-environment))
;|#
(driver-loop)



