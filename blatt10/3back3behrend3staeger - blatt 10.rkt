#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 10
  
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

; Prüft ob jedes Element in der Liste xs das Präfikat p? erfüllt
( define ( every p? xs )
   ( andmap p? xs )
   )

; Beispielaufruf
;(every (curry = 3) '(3 3 3))


; Prüft, ob mindestens ein Element der Liste cs das Prädikat p? erfüllt
( define ( some p? xs )
   ( ormap p? xs )
   )

; Beispielaufruf
;(some (curry = 3) '(1 3 9))

