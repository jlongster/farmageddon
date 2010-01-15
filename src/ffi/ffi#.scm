
(define-macro (with-alloc expr . rest)
  `(let (,expr)
     (let ((ret (begin ,@rest)))
       (free ,(car expr))
       ret)))
