(use-modules (srfi srfi-1) (ice-9 format))


(define (range from to by)
  (if (>= from to)
      '()
      (cons from (range (+ by from) to by))))

(define (column-conflicts c vec)
  (count (λ (v) (eqv? (vector-ref vec c) v)) (vector->list vec)))

(define test1 (make-vector 10 0))

(define (diagonal-conflics c0 vec)
  (let ((l (vector-length vec))
        (el (vector-ref vec c0))
        (cnt 0))
    (do ((i 0 [+ i 1]))
        ((>= i l))
      (when (= (abs (- el (vector-ref vec i)))
               (abs (- c0 i)))
        (set! cnt (+ 1 cnt))))
    cnt))

(define (verify vec)
  (let ((l (vector-length vec)))
    (and  (and-map (λ (x) (= 0 (- (column-conflicts x vec) 1))) (range 0 l 1))
          (and-map (λ (x) (= 0 (- (diagonal-conflics x vec) 1))) (range 0 l 1)))
    ))

(define (sort-error a b)
  (let ([a-err (cdr a)]
        [b-err (cdr b)])
    (cond [(> a-err b-err) #f]
          [(< a-err b-err) #t]
          [(= a-err b-err) (if (= 1 (random 2)) #t #f)])))

(define (minimize-conflicts r vec)
  (let* ([l (vector-length vec)]
         [vals (range 0 l 1)]
         [val (vector-ref vec r)]
         [error-table (map (λ (c) (cons c (begin
                                            (vector-set! vec r c)
                                            (+ (column-conflicts  r vec)
                                               (diagonal-conflics r vec)))))
                           vals)])
    (vector-set! vec r (caar (sort error-table sort-error)))))

(define (print-board board)
  (define (make-row n  i trgt fill)
    (let ((row (make-list n fill)))
      (list-set! row i trgt )
      row))
  (let* ((l (vector-length board))
	 (lst (map (λ (e) (make-row l e "Q" ".")) (vector->list board))))
    
    (for-each (λ (r) (format #t "~{~a~_ ~}~%" r)) lst)))

(define (n-queens max-iter board)
  (let ([prev-state (vector-copy board)]
        [no-change 0]
        [iter 0]
	[not-done #t])
    (while (and not-done
                (> max-iter iter))
      (set! not-done (not (verify board)))
      (set! iter (+ 1 iter ))
      ;; (set! prev-state (cons (vector-copy board) prev-state))
      (let ([random-number (random (vector-length board))])
        (minimize-conflicts random-number board)))
    (values iter prev-state board (not not-done))
   ))


(define (cc-test board )
  (call-with-values (lambda () (n-queens 5000 board)) (λ (iter init solution solved)
							(print-board init)
							(format #t "~%")
							(print-board solution)
							(format #t "A solution was ~a after ~a iterations.~%"
								(if solved "found" "not found") iter))))





