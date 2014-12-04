#lang racket

;Strg + \ --> Lambda

#|
Liste der tollen Dinge:

- Funktion höherer Ordnung
- lambda
- curry, curryr
- map, andmap, ormap
- filter
- foldl, foldr
apply
compose
findf, assf, count, partition
|#



;Eine Funktion höherer Ordnung erwartet als Parameter eine Funktion
;oder gibt eine Funktion aus.



(define plus3 (curry + 3))

(define x (curry < 4))

;((curry < 4) 5)
;--> (< 4 5)

;((curryr < 7) 6)
;--> (< 6 7)



;(and arg1 arg2 ... argx)
;--> Sind alle #t, wird das letzte Element ausgegeben.

;(or arg1 arg2 .. argx)
;--> Der erste Wert, der #t ist wird ausgegeben.



(filter even? '(1 2 3 4 5 6 7 8)) 
;--> '(2 4 6 8)



;(foldl + 1 '(2 3 4))
;--> (+ (+ (+ 1 2) 3) 4)
(foldl (curry * 2) 1 '(1 2 3))
;--> 48

;Listen umdrehen:
(foldl cons '() '(1 2 3))
;--> '(3 2 1)



(apply * 4 5 '(6 3))
;--> (* 4 5 6 3)
;--> 360

(apply + '(1 2 3 4))
;--> (+ 1 2 3 4)



(define (abssum xs)
  (apply + (map abs xs)))

(define (sieben xs) ;alle Vielfachen von 7 werden rausgeschmissen
  (filter (lambda x (not (zero? (modulo x 7))))
          xs))



(define (toll x)
  ((compose (curry * 7)
            (curry + 5))
   x))

;((compose f g) x)
;--> (f (g x))

;(compose (celsius->fahrenheit x MIT_CURRY?