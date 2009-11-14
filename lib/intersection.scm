
(declare (block)
         (standard-bindings)
         (extended-bindings))

;; May be better in C, who knows?

(define (ray-box-intersection x1 y1 z1
                              x2 y2 z2
                              orig-x orig-y orig-z
                              ray-x ray-y ray-z)
  (let ((numbers (vector x1 x2 orig-x ray-x
                         y1 y2 orig-y ray-y
                         z1 z2 orig-z ray-z))
        (tnear #f)
        (tfar #f))
    (let loop ((i 0))
      (if (>= i (vector-length numbers))
          #t
          (let ((n1 (vector-ref numbers i))
                (n2 (vector-ref numbers (+ i 1)))
                (orig-n (vector-ref numbers (+ i 2)))
                (ray-n (vector-ref numbers (+ i 3))))
            (if (and (eq? ray-n 0.)
                     (or (< orig-n n1)
                         (> orig-n n2)))
                #f
                (let ((t1 (/ (- n1 orig-n) ray-n))
                      (t2 (/ (- n2 orig-n) ray-n)))
                  (let ((t1 (min t1 t2))
                        (t2 (max t1 t2)))
                    (if (or (not tnear)
                            (> t1 tnear))
                        (set! tnear t1))
                    (if (or (not tfar)
                            (< t2 tfar))
                        (set! tfar t2))
                    (if (or (> tnear tfar)
                            (< tfar 0))
                        #f
                        (loop (+ i 4)))))))))))



;; (ray-box-intersection
;;  5. 5. 5.
;;  10. 10. 10.
;;  0. 0. 0.
;;  1. 1. 1.
;;  ) => #t

;; (ray-box-intersection
;;  5. 5. 5.
;;  10. 10. 10.
;;  0. 0. 0.
;;  10. 5. 10.
;;  ) => #t

;; (ray-box-intersection
;;  5. 5. 5.
;;  10. 10. 10.
;;  0. 0. 0.
;;  10. 4.99 10.
;;  ) => #f

;; (ray-box-intersection
;;  -5. -10. -10.
;;  5. 10. 10.
;;  0. 0. 0.
;;  -1. 1. 1.
;;  ) => #t

;; (ray-box-intersection
;;  -5. 2. 5.
;;  5. 10. 10.
;;  0. 0. 0.
;;  -5. 2. 5.
;;  ) => #f

;; (ray-box-intersection
;;  -5. 2. 5.
;;  5. 10. 10.
;;  0. 0. 0.
;;  -5. 2. 4.999
;;  ) => #f
