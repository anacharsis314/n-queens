;; This is my attempt at solving the n-queens problem in guile scheme.                                        ;;
;; It uses the min-conflict algorithm, but unlike stated by peter norvig, the solution time DOES depend on n. ;;
;; Maybe something is wrong in my code :)                                                                     ;;
;; On my old thinkpad it can solve up to 5000-queens before crashing or taking too long.                      ;;

(use-modules (srfi srfi-1) (ice-9 format))

(define (take-random lst)
  ;; randomly picks an element of [lst]
  (list-ref lst (random (length lst))))

(define (column-conflicts c0 board)
  ;; returns the number of column conflicts that occur with [c0] element of [board]
  (let ([cnt 0]
	[val (vector-ref board c0)])
    (do ((i 0 [+ i 1]))
	((>= i (vector-length board) ))
      (when (eqv? val (vector-ref board i))
	(set! cnt (+ 1 cnt))))
    cnt))

(define (any-column-conflicts c0 board)
  ;; returns #f if there are no conflicts with element [c0] of [board], #t when there are.
  (call/cc  (λ (return) (let ([val (vector-ref board c0)])
			  (do ((i 0 [+ 1 i]))
			      ((>= i (vector-length board)))
			    (let ([el (vector-ref board i)])
			      (when (and (not (= i c0)) (= val el)) (return #t)))
			    ) #f))))

(define (diagonal-conflicts c0 board)
  ;; returns the number of diagonal conflicts for the position [c0] in [board]
  (let ((l (vector-length board))
        (val (vector-ref board c0))
        (cnt 0))
    (do ((i 0 [+ i 1]))
        ((>= i l))
      (when (= (abs (- val (vector-ref board i)))
               (abs (- c0 i)))
        (set! cnt (+ 1 cnt))))
    cnt))

(define (any-diagonal-conflicts c0 board)
  ;; returns #f when there are no conflicts for the element [c0] of [board], #t when there are.
  (call/cc
   (λ (return)
     (let ((l (vector-length board))
           (val (vector-ref board c0)))
       (do ((i 0 [+ i 1]))
           ((>= i l))
	 (when (and (not (= i c0))
		    (= (abs (- val (vector-ref board i)))
		       (abs (- c0 i))))
	   (return #t)))
       #f))))

(define (total-conflicts c0 board)
  ;; total conflicts for queen [c] at [board] 
  (+ (column-conflicts  c0 board)
     (diagonal-conflicts c0 board)))

(define (sort-conflict a b)
  ;; used to compare  elements [a] and [b] of type (number . total-conflicts) based on total conflicts.
  ;; breaks ties at random.
  (let ([conflict-A (cdr a)]
        [conflict-B (cdr b)])
    (cond [(> conflict-A conflict-B) #f]
          [(< conflict-A conflict-B) #t]
          [(= conflict-A conflict-B) (if (= 1 (random 2)) #t #f)])))

(define (minimize-conflicts r board)
  ;; given a queen [r] and a [board], selects a new position that minimizes the column and diagonal conflicts
  ;; destrutively updates [board]
  (let* ([l (vector-length board)]
         [vals (iota l 0 1)]
         [val (vector-ref board r)]
         [conflict-table (map (λ (c) (cons c (begin
                                               (vector-set! board r c)
                                               (total-conflicts r board ))))
                              vals)])
    (vector-set! board r (caar (sort conflict-table sort-conflict)))))

(define (print-board board)
  ;; pretty prints [board]
  (define (make-row n  i trgt fill)
    (let ((row (make-list n fill)))
      (list-set! row i trgt )
      row))
  (let* ((l (vector-length board))
	 (lst (map (λ (e) (make-row l e "Q" ".")) (vector->list board))))
    
    (for-each (λ (r) (format #t "~{~a~_ ~}~%" r)) lst)))

(define (conflict-idx board)
  ;; returns a list of indices with conflicts for a given [board]
  (let* ([l (vector-length board)]
	 [tmp '()])
    (do ((i 0 [+ 1 i]))
	((>= i l))
      (when (or (any-column-conflicts i board)
		(any-diagonal-conflicts i board))
	(set! tmp (cons i tmp))))
    tmp))


(define (n-queens max-iter board)
  ;; solves the problem given an initial state [board] and a max iteration limit [max-iter].
  ;; output is in the form of (values iterations initial-state final-state solved?), 
 
  (let ([prev-state (vector-copy board)]
        [no-change 0]
        [iter 0]
	[done #f]
	[list-of-conflicting-indices '()])
    ;; populate the list of conflicting element list
    (set! list-of-conflicting-indices (conflict-idx board))

    ;; set not done
    (set! done (not (null? list-of-conflicting-indices)))

    ;; first minimization
    (for-each (λ (e) (minimize-conflicts e board)) list-of-conflicting-indices)

    ;; while the list with conflicting indexes is not empty or max-iters is not reached, minimize rows
    (while (> max-iter iter)
      ;; update not-done
      (set! done (null? list-of-conflicting-indices))
      ;; break when condition reached
      (when done (break))
      ;; inc iterations
      (set! iter (+ 1 iter ))
      ;; minimize conflicts in a random conflicting row
      (let ([random-number (take-random list-of-conflicting-indices)])
	(minimize-conflicts random-number board))
      ;; recalculate conflicts after change
      (set! list-of-conflicting-indices (conflict-idx board)))
    ;;return initial state, iterations, the final state and if the process finished
    (values iter prev-state board done)))


(define (cc-test board )
  (call-with-values (lambda () (n-queens 5000 board)) (λ (iter init solution solved)
							(print-board init)
							(format #t "~%")
							(print-board solution)
							(format #t "A solution was ~a after ~a iterations.~%"
								(if solved "found" "not found") iter))))
