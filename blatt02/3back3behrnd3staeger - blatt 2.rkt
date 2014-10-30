#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  Aufgabenblatt 02
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#



; ##############################################################################
; ## Aufgabe 1.1 ###############################################################
; ##############################################################################

(define miau 'Plueschi )
(define katze miau)
(define tiger 'miau)
(define (welcherNameGiltWo PersonA PersonB)
  (
   let
      (
       (PersonA 'Sam)
       (PersonC PersonA )
       )
    PersonC
    )
  )

(define xs1 '(0 2 3 miau katze ))
(define xs2 (list miau katze ))
(define xs3 (cons katze miau ))

miau
katze
tiger
(quote katze)
;(eval tiger)
;(eval katze)
;(eval 'tiger)
(welcherNameGiltWo 'harry 'potter)
(cdddr xs1)
(cdr xs2)
(cdr xs3)
;(eval (sqrt 3))
