#lang racket

;; 判断是否以 tag开头
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;;
(define (my-eval exp evn)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (my-eval (cond->if exp) env)]
        [(application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type -- EVAL" exp)]
  ))

(define (my-apply procedure arguments)
  (cond [(primitive-procedure? procedure)    ;; 基本过程
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)     ;; 组合过程
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-a\parameters procedure)
                              arguments
                              (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY" procedure)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 过程参数
(define (list-of-values exps env)
  (if (no-operands exps)
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
(define (assignemnt-value exp) (caddr exp))

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
(define (begin? exp) (tagged-list? 'begin))
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
(define (operands exp) (cd exp))
(define (no-operands? ops) (null? ops))
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
(define (cond? exp) (tagged-list? 'cond))
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
(define (let? exp) (tagged-list? 'let))
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
(define (apply-primitive-procedure proc args)
  '()) ;; todo
(define (primitive_procedure? proc)
  '()) ;; todo

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 对环竟的操作
()












