
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
