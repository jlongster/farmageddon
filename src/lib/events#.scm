
(define-macro (define-event-handler sig . body)
  (let ((name (car sig))
        (args (cdr sig)))
    `(install-event-handler
      ',name
      (lambda ,args ,@body))))
