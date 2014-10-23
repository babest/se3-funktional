#lang racket

; Aufgabe 1.1
; -----------

; Berechnte den Radiant aus den Grand
; Beispielaufruf: (degrees->radians 360)
(define (degrees->radians degrees)
  ( * ( / degrees 180 ) pi )
)

; Berechnet die Grad aus dem Raidant
; Beispielaufruf: (radians->degrees 3.141)
(define (radians->degrees radians)
  ( * ( / radians pi ) 180 )
)

; Aufgabe 1.2
; -----------

; Berechnet den acos von einem cos-Wert
; Beispielaufruf: (my-acos 1)
(define (my-acos cosvalue)
  ( atan 
    ( /
      ( getsinvalue cosvalue )
      cosvalue
    )
  )
)
; Hilfsmethode
; Berechnet den sin-Wert aus einem cos-Wert
(define (getsinvalue cosvalue)
  ( sqrt
    ( -
      1
      ( sqr cosvalue )
    )
  )
)

; Aufgabe 1.3
; -----------

; Rechnet Seemeilen in Kilometer um
; Beispielaufruf: (nm->km 10)
(define (nm->km nms)
  ( *
    nms
    1.852
  )
)

; Aufgabe 2.1
; -----------

