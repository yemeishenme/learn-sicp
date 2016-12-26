Welcome to Racket v6.7.
﻿> 


;;; M-Eval input:
(define (unless condition usual-value exceptional-value) (if condition exceptional-value usual-value))

;;; M-Eval output:
ok

;;; M-Eval input:
(unless (= b 0) (/ a b) 1)
; Unbound variable =

﻿> 


;;; M-Eval input:

(define (unless condition usual-value exceptional-value) (if condition exceptional-value usual-value))

;;; M-Eval output:
ok

;;; M-Eval input:
(unless true (/ 1 0) 'aaa)
; /: division by zero

﻿> 


;;; M-Eval input:

(define (unless condition usual-value exceptional-value) (if condition exceptional-value usual-value))

;;; M-Eval output:
ok

;;; M-Eval input:
(unless false (/ 1 0) 'aaa)
; /: division by zero

﻿> 