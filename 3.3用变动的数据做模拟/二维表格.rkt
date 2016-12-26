#lang racket
(require sicp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 二维表格
#|
;|#

;; 在二维表格中查找
(define (lookup key-1 key-2 table)
  (let ([subtable (assoc key-1 (cdr table))])
    (if subtable
        (let ([record (assoc key-2 (cdr subtable))])
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr subtable))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-dr! subtable
                       (cons (cons key-2 value)
                             (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
    'ok)







