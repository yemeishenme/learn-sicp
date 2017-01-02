 #lang racket
(require sicp)

;; 求值器的结构
(define (amb? exp) (tagged-list? exp 'amb));
;; 取 后面的部分
(define (amb-choices exp) (cdr exp))

;; 最高层的 ambeval
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; 最高层的 my-eval
(define (my-eval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;辅助函数
;; 判断是否以 tag开头
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 分析函数
(define (analyze exp)
  (cond
    ; 自求值表达式
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    ; 变量
    [(variable? exp) (analyze-variable exp)]
    ; 引号
    [(quoted? exp) (analyze-quoted exp)]
    ; 赋值
    [(assignment? exp) (analyze-assignment exp)]
    ; 定义
    [(definition? exp) (analyze-definition exp)]
    ; if
    [(if? exp) (analyze-if exp)]
    ; lambda
    [(lambda? exp) (analyze-lambda exp)]
    ; begin
    [(begin? exp) (analyze-sequence (begin-actions exp))]
    ; cond
    [(cond? exp) (analyze (cond->if exp))]
    ; let 后增加的情况
    [(let? exp) (analyze (let->lambda exp))]
    ; 非确定性求值
    ((amb? exp) (analyze-amb exp))
    ; 函数
    [(application? exp) (analyze-application exp)]
    ; 其它情况，报错
    [else
     (error "Unknown expression type -- ANALYZE" exp)]))

;;;;;; 以下是语法分析部分
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let [(var (assignment-variable exp))
        (vproc (analyze (assignment-value exp)))]
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let [(old-value
                      (lookup-variable-value var env))]
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let [(var (definition-variable exp))
        (vproc (analyze (definition-value exp)))]
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let [(pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp)))]     ; 先取得各个部分
    (lambda (env succeed fail)                      ; 函数必须是这种形式
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-lambda exp)
  (let [(vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp)))]
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  ;;
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  ;;
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  ;;
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- AAALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

;; 分析 amb 表达式
(define (analyze-amb exp)
  (let [(cprocs (map analyze (amb-choices exp)))] ; 获取表达式的列表
    (lambda (env succeed fail)
      ;; 定义函数
      (define (try-next choices)
        (if (null? choices)
            ; 为空
            (fail)
            ;; 否则
            ((car choices)  ;; 取得第一个
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      ;; 尝试表达式列表
      (try-next cprocs))))


(define (execute-application proc args succeed fail)
  (cond [(primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail)]
        [else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc)]))
;; 语法分析部结束
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (list 'list list)
        (list 'member member)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list 'not not)
        (list 'eq? eq?)
        (list 'display display)
        (list 'error error)
        (list 'abs abs)
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
(define input-prompt  ";;; Amb-Eval input<")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let [(input (read))]
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; 成功
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; fail
                     (lambda ()
                       (announce-output ";;; There are no more values of ")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

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
;; 启动求值器
(driver-loop)

#|
4.3.1 amb和搜索

(amb <e1> <e2> ... <en>)
    (list (amb 1 2 3) (amb 'a 'b)) =>
可能的返回结果：(1 a) (1 b) (2 a) (2 b) (3 a) (3 b)

|#
#|
TODO  这个函数怎么实现啊
;|#
;(define (amb ...))

#|
;; (Amb) : 这一计算会流失败，且不会产生任何值
;; 某个特定谓词必须为真
(define (require p)
  (if (not p) (amb)))


;;
(define (an-element-of items)
  ;; 如果空表，则会失败
  (require (not (null? items)))
  ;; 返回表里的第一个，或其它元素
  (amb (car items) (an-element-of (cdr items))))

;; 任何一个>=n的数
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
;|#

#|
;; 练习4.35
;; 返回两个限界之间的一个整数
(define (an-integer-between m n)
  (require (< m n))
  (amb m (an-integer-between (+ m 1) n)))
;; 寻找毕达哥拉斯三元组
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))         ;; 可以用let*解决？
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
;|#


#|

4.3.2 非确定程序的实例

(define (require p)
  (if (not p) (amb)))

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))

    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper )
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (distinct? items)
  (cond [(null? items) true]
        [(null? (cdr items)) true]
        [(member (car items) (cdr items)) false]
        [else (distinct? (cdr items))]))

;|#
;; 求值表达式，将产生下面的结果:
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

#|
自然语言的语法分析
... ... 略
;|#


;; 执行过程的一般行形式是
#|
(lambda (env succeed fail)
  ;; succeed is (lambda (value fail) ...)
  ;; fail    is (lambda () ...)
  ...)

(ambeval <exp>
         the-global-environment
         (lambda (value fail) value)
         (Lambda () 'failed))
;|#

