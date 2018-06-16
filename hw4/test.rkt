; Name: Beier (Benjamin) Liu
; Date: 5/17/2018

; Remark:

#lang racket

(provide (all-defined-out))
; ===================================================================================================
; File content:
; Write comments
; ===================================================================================================

(define s "hello world")

(define x 3)
(define y (+ x 2))

(define cube1
	(lambda (x)
		(* x (* x x))))

(define (cube2 x)
	(* x x x))

(define (pow x y)
	(if (= y 0)
		1
		(* x (pow x (- y 1)))))
