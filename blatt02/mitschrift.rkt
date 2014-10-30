#lang racket

(define liste1 '(1 2 3 4))
(define liste2 (list 1 2 3 4))

;###########
;#Rekursion#
;###########

(define (laenge liste)
  (if (null? liste)
      0
      (add1 (laenge (cdr liste))))) ;car gibt erstes Element aus (Rest fällt weg), cdr alles außer das erste (erstes Element fällt weg)

(define (länge liste acc)
  (if (null? liste)
      acc
      (länge (cdr liste) (add1 acc))))