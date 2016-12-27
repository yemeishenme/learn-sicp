#lang racket
(require sicp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一维表格
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

#|
((key val) (key val) ...) 根据key 这样的表中寻找val找到则返回该键值对闭，否则返回false
;|#
(define (assoc key records)
  (cond [(null? records) false]    ; 空表直接返回false
        [(equal? key (caar records)) ; 判断是否等于第一个表的第一个元素
         (car records)]
        [else (assoc key (cdr records))]))

#|
向表中插入键值对儿
;|#
(define (insert! key value table)
  (let ((record (assoc key (cdr table)))) ; 查找对应的值
    (if record                            ; 如查找到
        (set-cdr! record value)           ; 设置其值为value
        (set-cdr! table                   ; 将该键值对儿接在表的开头位置(*table*的后面)
                  (cons (cons key value) (cdr table)))))
  'ok)

;#|
(define (insert-tail-t! key value table)
  (let ((record (assoc key (cdr table)))) ; 查找对应的值
    (if record                            ; 如查找到
        (set-cdr! record value)           ; 设置其值为value
        (if (<= (length table) 1)         ; 如果表的长度小于等于1，说明表中没有元素
            (set-cdr! table
                      (cons (cons key value)))
            (append table                   ; 将该键值对儿接在表的开头位置
                      (cons key value)))))
  'ok)
;|#
#|
构造新表:创建一个包含符号*table*的表
;|#
(define (make-table)
  (list '*table*))
