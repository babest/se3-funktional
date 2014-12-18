#lang racket

#|

(neue-funktion + '((1 2 3) (4 5 6)))
--> '((5 6 7) (6 7 8) (7 8 9))

(neue-funktion * '((1 2 3) (4 5 6)))
--> '((4 5 6) (8 10 12) (12 15 18))

(neue-funktion < '((1 3 5 7) (2 4 6)))
--> '((#t #t #t) (#f #t #t) (#f #f #t) (#f #f #f))

|#

(define (neue-funktion func liste)
  (map 
   
   (lambda (x)
     (map 
      (curry func x) 
      (cadr liste)
      )
     )
   
   (car liste)
   )
  )

(neue-funktion * '((1 2 3) (4 5 6)))

;parallele Programmierung: future, touch