#lang racket

#|
- Fakultät mit FHOs
  (nat 5) -> '(0 1 2 3 4)

- Fibonacci nicht baumrekursiv
  fib(0) := 0, fib(1) := 1, fib(i) := fib(i-1) + fib(i-2)

- Produktliste
  (produktliste '(1 4 2) 3)
  -> '(3 12 6)

  - rekursiv
  - endrekursiv
  - FHO

- Curry implementieren
  (define (f . argliste))
  (lamdda argliste)
|#



(define (nat n)
	(build-list n identity))

(define (fak1 n)
	(foldl (* 1 (map add1 nat(n)))))

(define (fak2 n)
	(apply * (map add1 (nat n))))

(define (fib n)
	(define (helper c f1 f2)
		(cond [(zero? c) f1]
			  [else (helper (sub1 c) f2 (+ f1 f2))]))
	(helper n 0 1))

(define (produkt1 liste faktor)
	(map (curry * faktor) liste))

(define (produkt2 liste faktor)
	(if (null? liste)
		'()
		(cons (* (car liste) faktor) (produkt2 (cdr liste) faktor))))

(define (produkt3 liste faktor)
	(define (helper acc liste)
		(if (null? liste) (reverse acc)
			(helper (cons (* faktor (car liste)) acc) (cdr liste))))
	(helper '() liste))

(define (my-curry1 f . args)
	(lambda new-args (apply f (append args new-args))))