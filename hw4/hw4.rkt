; Name: Beier (Benjamin) Liu
; Date: 5/17/2018

; Remark:

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; ===================================================================================================
; File content:
; Write comments
; ===================================================================================================

;; put your code below

; Q1
; Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers.
; Further assume stride is positive. sequence produces a list of numbers from low to high (including
; low and possibly high) separated by stride and in sorted order. Sample solution: 4 lines
(define (sequence low high stride)
	(if (> low high)
		null
		(cons low (sequence (+ low stride) high stride))))

; Q2
; Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
; list of strings. Each element of the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix). You must use Racket-library
; functions map and string-append. Sample solution: 2 lines
(define (string-append-map xs sfx)
	(map
		(lambda (s) (string-append s sfx))
		xs))

; Q3
; Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
; empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the ith
; element of the list where we count from zero and i is the remainder produced when dividing n by the
; list's length. Library functions length, remainder, car, and list-tail are all useful { see the Racket
; documentation. Sample solution is 6 lines
(define (list-nth-mod xs n)
	(cond [(< n 0) (error "list-nth-mod: negative number")]
		[(null? xs) (error "list-nth-mod: empty list")]
		[#t (car (list-tail xs (remainder n (length xs))))]))

; Q4
; Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
; the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines
(define (stream-for-n-steps s n)
	(letrec ([aux (lambda (s cnt acc)
						(let* ([thisone (s)])
							(if (<= cnt 0)
								acc
								(aux (cdr thisone) (- cnt 1) (cons (car thisone) acc)))))])
	(reverse (aux s n null)) ))

; Q5
; Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
; except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream
; is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will
; be another stream
(define funny-number-stream
	(letrec ([aux (lambda (x)
						(if (= (modulo x 5) 0)
							(cons (- 0 x) (lambda () (aux (+ x 1))))
							(cons x (lambda () (aux (+ x 1))))))])
	(lambda () (aux 1))))

; Q6
; Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called... etc. Sample solution: 4 lines
(define dan-then-dog
	(letrec ([aux (lambda (x)
						(if (= (modulo x 2) 0)
							(cons "dog.jpg" (lambda () (aux (+ x 1))))
							(cons "dan.jpg" (lambda () (aux (+ x 1))))))])
	(lambda () (aux 1))))


; Q7
; Write a function stream-add-zero that takes a stream s and returns another stream. If s would
; produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v) for its
; ith element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
; with place-repeatedly
(define (stream-add-zero s)
	(letrec ([aux (lambda (s)
						(let* ([thisone (s)])
							(lambda () (cons (cons 0 (car thisone)) (aux (cdr thisone)) )) ))])
	(aux s)))


; Q8
; Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or
; may not be the same length, but assume they are both non-empty. The elements produced by the
; stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
; forever through the lists. For example, if xs is '(1 2 3) and ys is '("a" "b"), then the stream
; would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"),
; (2 . "b"), etc
; Sample solution is 6 lines and is more complicated than the previous stream problems. Hints: Use one
; of the functions you wrote earlier. Use a recursive helper function that takes a number n and calls
; itself with (+ n 1) inside a thunk
(define (cycle-lists xs ys)
	(letrec ([aux (lambda (cnt) (lambda () (cons (cons (list-nth-mod xs cnt) (list-nth-mod ys cnt)) (aux (+ cnt 1)))))])
	(aux 0)))


; Q9
; Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket's
; assoc library function except (1) it processes a vector (Racket's name for an array) instead of a list,
; (2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly
; two arguments. Process the vector elements in order starting from 0. You must use library functions
; vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
; equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
; recursive helper function
(define (vector-assoc v vec)
	(letrec ([aux (lambda (cnt)
			(cond [(= (vector-length vec) cnt) #f]
				[(pair? (vector-ref vec cnt))
				(if (equal? (car (vector-ref vec cnt)) v)
					(vector-ref vec cnt)
					(aux (+ cnt 1)))]
				[#t (aux (+ cnt 1))]))])
	(aux 0)))


; Q10
; Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
; one argument v and returns the same thing that (assoc v xs) would return. However, you should
; use an n-element cache of recent results to possibly make this function faster than just calling assoc
; (if xs is long and a few elements are returned often). The cache must be a Racket vector of length n
; that is created by the call to cached-assoc (use Racket library function vector or make-vector) and
; used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is
; positive.
; The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it
; first checks the cache for the answer. If it is not there, it uses assoc and xs to get the answer and if
; the result is not #f (i.e., xs has a pair that matches), it adds the pair to the cache before returning
; (using vector-set!). The cache slots are used in a round-robin fashion: the first time a pair is added
; to the cache it is put in position 0, the next pair is put in position 1, etc. up to position n 􀀀 1 and
; then back to position 0 (replacing the pair already there), then position 1, etc
(define (cached-assoc xs n)
		(letrec
			([cache (make-vector n)]
			[cache-slot 0]
			[aux (lambda(x)
					(let ([ans (vector-assoc x cache)])
						(if ans
							ans
							(let ([new-ans (assoc x xs)])
							(if new-ans
								(begin
									(vector-set! cache cache-slot new-ans)
									(set! cache-slot (remainder (+ cache-slot 1) n))
									new-ans)
								new-ans)))))])
		aux))

; Q11
; Define a macro that is used like (while-less e1 do e2) where e1 and e2
; are expressions and while-less and do are syntax (keywords)
(define-syntax while-less
	(syntax-rules (do)
		[(while-less e1 do e2)
		(letrec ([t1 (eval e1)]
				[loop (lambda (t2)
							(if (>= t2 t1)
								#t
								(loop e2)))])
		(loop e2))]))
