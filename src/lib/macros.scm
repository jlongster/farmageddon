
(define-macro (for-expansion expr)
  (eval expr))

(for-expansion
 (define (symbol-append . syms)
   (string->symbol
    (apply string-append
           (map (lambda (sym)
                  (if (symbol? sym)
                      (symbol->string sym)
                      sym))
                syms)))))

(define-macro (expand-eval expr)
  (eval expr))

(define-macro (define-expand-var name value)
  `(expand-eval (define ,name ,value)))

(define-macro (expand-if cond then . else)
  `(expand-eval (if ,cond
                    ',then
                    ',(if (null? else) (void) (car else)))))
