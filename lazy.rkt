#lang plait #:lazy

(define (take-while p l)
  (cond
    [(empty? l) empty]
    [(p (first l)) (cons (first l) (take-while p (rest l)))]
    [else empty]))

(define helper (cons 0 (map add1 helper)))

(define (build-infinite-list f)
  (map f helper))

(define (prime?/fast n)
  (cond
    [(= n 2) #true]
    [(< n 2) #false]
    [else (empty? (filter (lambda (x) (= (remainder n x) 0)) (take-while (lambda (v) (<= (* v 2) n)) primes/fast)))]))


(define primes/fast (filter (lambda (n) (prime?/fast n)) (build-infinite-list (lambda (n) (+ 1 n)))))

(define (build-table rows cols f)
  (build-list rows (lambda (row) (build-list cols (lambda (col) (f row col))))))


(define (lcs-length s1 s2)
  (letrec ([table (build-table (+ 1 (string-length s1)) (+ 1 (string-length s2)) (lambda (row col) (cond
                                                                                                  [(= row 0) 0]
                                                                                                  [(= col 0) 0]
                                                                                                  [else (if (char=? (string-ref s1 (- row 1)) (string-ref s2 (- col 1)))
                                                                                                            (+ (list-ref (list-ref table (- row 1)) (- col 1)) 1)
                                                                                                            (max (list-ref (list-ref table row) (- col 1))
                                                                                                                 (list-ref (list-ref table (- row 1)) col)))])))])
                                                                                                                 
    (list-ref (list-ref table (string-length s1)) (string-length s2))))
     










  