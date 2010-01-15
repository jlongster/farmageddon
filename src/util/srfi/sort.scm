
(define (sort-list lst less?)

  (define (mergesort lst)

    (define (merge lst1 lst2)
      (cond ((null? lst1) lst2)
            ((null? lst2) lst1)
            (else
             (let ((e1 (car lst1)) (e2 (car lst2)))
               (if (less? e1 e2)
                   (cons e1 (merge (cdr lst1) lst2))
                   (cons e2 (merge lst1 (cdr lst2))))))))

    (define (split lst)
      (if (or (null? lst) (null? (cdr lst)))
          lst
          (cons (car lst) (split (cddr lst)))))

    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ((lst1 (mergesort (split lst)))
               (lst2 (mergesort (split (cdr lst)))))
          (merge lst1 lst2))))

  (mergesort lst))
