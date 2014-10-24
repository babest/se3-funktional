#lang racket

#|
  SE3 Funktionale Programmierung Uni Hamburg
  Aufgabenblatt 01
  Eingereicht von
    Timon Back (3back)
    Fabien Behrendt (3behrend)
    Nicolai Stäger (3staeger)
|#


; Aufgabe 1.1
; ------------------------------------------------------------------------------

; Berechnte den Radiant aus den Grand
; Beispielaufruf:
;   (degrees->radians 360)
(define (degrees->radians degrees)
  ( *
    ( /
      degrees
      180
    )
    pi
  )
)

; Berechnet die Grad aus dem Raidant
; Beispielaufruf:
;   (radians->degrees 3.1415926)
(define (radians->degrees radians)
  ( *
    ( /
      radians
      pi
    )
    180
  )
)


; Aufgabe 1.2
; ------------------------------------------------------------------------------

; Berechnet den acos von einem cos-Wert
;   - aus dem Cosinus wird der Sinus berechnet
;   - der Sinus wird durch den Cosinus geteilt
;   - daraus wird der Arcustan gebildet
; Beispielaufruf:
;   (my-acos 1)
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
;   - Quadriert den Cosinuswert
;   - Subtrahiert dies von 1
;   - Zieht die Wurzel
; sin^2 x + cos^2 x = 1
; -> sin = sqrt ( 1 - cos^2 x )
(define (getsinvalue cosvalue)
  ( sqrt
    ( -
      1
      ( sqr cosvalue )
    )
  )
)


; Aufgabe 1.3
; ------------------------------------------------------------------------------

; Rechnet Seemeilen in Kilometer um
;   - 1 nm = 1.852 km
; Beispielaufruf:
;   (nm->km 10)
(define (nm->km nms)
  ( *
    nms
    1.852
  )
)


; Aufgabe 2.1
; ------------------------------------------------------------------------------

; Berechnet die Entfernung zweier Punkte auf der Erde in Kilometer
; Beispielaufruf:
;   Oslo - Honkong           : (distanzAB 59.93 10.75 22.2 114.1)
;   San Francisco - Honolulu : (distanzAB 37.75 -122.45 21.32 -157.83)
;   Osterinseln - Lima       : (distanzAB -27.1 -109.4 -12.1 -77.05)
(define (distanzAB breitengradA laengengradA breitengradB laengengradB)
  ; von Seemeilen in Kilometer umrechnen
  ( nm->km
    ; Ausrechnen der Minuten
    ( *
      ; Berechnung von d_g in Grad
      ( radians->degrees
        ; Berechnung von cos(d_g)
        ( zentriWinkel
          breitengradA
          laengengradA
          breitengradB
          laengengradB
        )
      )
      60
    )
  )
)
; Berechnet den Zentriwinkel (d_g) anhand zweier Positionen in Rad
(define (zentriWinkel breitengradA laengengradA breitengradB laengengradB)
  ( my-acos
    ( +
      ( *
        ( sin
          ( degrees->radians breitengradA )
        )
        ( sin
          ( degrees->radians breitengradB ) 
        )
      )
      ( *
        ( cos
          ( degrees->radians breitengradA )
        )
        ( cos
          ( degrees->radians breitengradB )
        )
        ( cos
          ( degrees->radians
            ( - laengengradB laengengradA )
          )
        )
      )
    )
  )
)


; Aufgabe 2.2
; ------------------------------------------------------------------------------

; Berechnet die Richtung, in der ein Ziel B vom Punkt A aus liegt
; anhand der Funktion:
;   acos( ( sin(br B) - cos(zentriwinkel) * sin(br A) ) / ( cos(br A) * sin(zentriwinkel) ) )
; Beispielaufruf:
;   Oslo - Honkong     : (anfangskurs 59.93 10.75 22.2 114.1)
(define (anfangskurs breitengradA laengengradA breitengradB laengengradB)
  ( radians->degrees  
    ( my-acos
      ( /
        ( -
          ( sin 
            ( degrees->radians breitengradB )
          )
          ( *
            ( cos
              ( zentriWinkel
                breitengradA
                laengengradA
                breitengradB
                laengengradB
              )
            )
            ( sin
              ( degrees->radians breitengradA )
            )
          )
        )
        ( *
          ( cos
            ( degrees->radians breitengradA )
          )
          ( sin
            ( zentriWinkel
              breitengradA
              laengengradA
              breitengradB
              laengengradB
            )
          )
        )
      )
    )
  )
)


; Aufgabe 2.3
; ------------------------------------------------------------------------------

(define (grad->himmelsrichtung grad)
  ( grad )
)

