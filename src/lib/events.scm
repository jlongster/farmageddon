;;;; Events

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define %%handlers (make-table))

(define (install-event-handler name handler)
  (table-set!
   %%handlers
   name
   (cons handler (table-ref %%handlers name '()))))

(define (run-event-handlers name . args)
  (for-each (lambda (handler)
              (apply handler args))
            (table-ref %%handlers name '())))

;;;; Usage

;;; (install-event-handler 'event-name (lambda (event) ...))
;;; (define-event-handler (event-name event) ...)
;;; (run-event-handlers 'event-name)
