#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  Aufgabenblatt 01
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

;; Berechnte den Radiant aus den Grand
(define (degrees->radians degrees)
  ( *
    ( /
      degrees
      180
    )
    pi
  )
)

;; Berechnet die Grad aus dem Raidant
(define (radians->degrees radians)
  ( *
    ( /
      radians
      pi
    )
    180
  )
)

; Beispielaufruf:
; ( degrees->radians 360 )
; ( radians->degrees 3.1415926 )




; ##############################################################################
; ## Aufgabe 1.2 ###############################################################
; ##############################################################################

;; Berechnet den acos von einem cos-Wert
;;   - aus dem Cosinus wird der Sinus berechnet
;;   - der Sinus wird durch den Cosinus geteilt
;;   - daraus wird der Arcustan gebildet
(define (my-acos cosvalue)
  ( atan 
    ( /
      ( getsinvalue cosvalue )
      cosvalue
    )
  )
)

;; Hilfsmethode
;; Berechnet den sin-Wert aus einem cos-Wert
;;   - Quadriert den Cosinuswert
;;   - Subtrahiert dies von 1
;;   - Zieht die Wurzel
;; Grundlegende Funktion:
;; sin^2 x + cos^2 x = 1
;; -> sin = sqrt ( 1 - cos^2 x )
(define (getsinvalue cosvalue)
  ( sqrt
    ( -
      1
      ( sqr cosvalue )
    )
  )
)

; Beispielaufruf:
; ( my-acos 1 )




; ##############################################################################
; ## Aufgabe 1.3 ###############################################################
; ##############################################################################

;; Rechnet Seemeilen in Kilometer um
;;   - 1 nm = 1.852 km
(define (nm->km nms)
  ( *
    nms
    1.852
  )
)

; Beispielaufruf:
; (nm->km 10)




; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

;; Berechnet die Entfernung zweier Punkte auf der Erde in Kilometer
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

; Beispielaufruf:
; ( distanzAB  59.93   10.75  22.20  114.10 )  ; Oslo - Honkong
; ( distanzAB  37.75 -122.45  21.32 -157.83 )  ; San Francisco - Honolulu
; ( distanzAB -27.10 -109.40 -12.10  -77.05 )  ; Osterinseln - Lima




; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################

;; Berechnet die Richtung, in der ein Ziel B vom Punkt A aus liegt
(define (anfangskurs breitengradA laengengradA breitengradB laengengradB)
  ( let ( [winkel (richtungswinkel breitengradA laengengradA breitengradB laengengradB)] )
    winkel
  )
)

;; Hilfsmethode
;; Berechnet den Richtungswinkel von Punkt A Richtung Punkt B
;; anhand der Funktion:
;;   acos( ( sin(br B) - cos(zentriwinkel) * sin(br A) ) / ( cos(br A) * sin(zentriwinkel) ) )
(define (richtungswinkel breitengradA laengengradA breitengradB laengengradB)
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

; Beispielaufruf:
; ( anfangskurs  59.93   10.75  22.20  114.10 )  ; Oslo - Honkong
; ( anfangskurs  37.75 -122.45  21.32 -157.83 )  ; San Francisco - Honolulu
; ( anfangskurs -27.10 -109.40 -12.10  -77.05 )  ; Osterinseln - Lima




; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################

;; Konvertiert eine Gradzahl in eine Himmelsrichtung
(define (grad->himmelsrichtung grad)
  ( cond [ ( > grad 360 ) ( error "Gradzahl ist zu Groß" ) ]
         [ ( or ( <= grad 11.25 ) ( > grad 348.75 ) ) "N" ]
         [ ( <= grad 33.75 )  "NNE" ]
         [ ( <= grad 56.25 )  "NE" ]
         [ ( <= grad 78.75 )  "ENE" ]
         [ ( <= grad 101.25 ) "E" ]
         [ ( <= grad 123.75 ) "ESE" ]
         [ ( <= grad 146.25 ) "SE" ]
         [ ( <= grad 168.75 ) "SSE" ]
         [ ( <= grad 191.25 ) "S" ]
         [ ( <= grad 213.75 ) "SSW" ]
         [ ( <= grad 236.25 ) "SW" ]
         [ ( <= grad 258.75 ) "WSW" ]
         [ ( <= grad 281.25 ) "W" ]
         [ ( <= grad 303.75 ) "WNW" ]
         [ ( <= grad 326.25 ) "NW" ]
         [ ( <= grad 348.75 ) "NNW" ]
  )
)

;; Konvertiert eine Himmelsrichtung zu einer Gradzahl
(define (himmelsrichtung->grad himmelsrichtung)
  ( cond [ ( string=? himmelsrichtung "N" )   0     ]
         [ ( string=? himmelsrichtung "NNE" ) 22.5  ]
         [ ( string=? himmelsrichtung "NE" )  45    ]
         [ ( string=? himmelsrichtung "ENE" ) 67.5  ]
         [ ( string=? himmelsrichtung "E" )   90    ]
         [ ( string=? himmelsrichtung "ESE" ) 112.5 ]
         [ ( string=? himmelsrichtung "SE" )  135   ]
         [ ( string=? himmelsrichtung "SSE" ) 157.5 ]
         [ ( string=? himmelsrichtung "S" )   180   ]
         [ ( string=? himmelsrichtung "SSW" ) 202.5 ]
         [ ( string=? himmelsrichtung "SW" )  225   ]
         [ ( string=? himmelsrichtung "WSW" ) 247.5 ]
         [ ( string=? himmelsrichtung "W" )   270   ]
         [ ( string=? himmelsrichtung "WNW" ) 292.5 ]
         [ ( string=? himmelsrichtung "NW" )  315   ]
         [ ( string=? himmelsrichtung "NNW" ) 337.5 ]
  )
)

; Beispielaufruf:
; ( displayln ( grad->himmelsrichtung 0 ) )
; ( for ( [i 16] )
;    ( displayln ( grad->himmelsrichtung ( * 22.5 i ) ) )
; )
; ( displayln ( himmelsrichtung->grad "N" ) )
; ( for ( [i 16] )
;    ( displayln ( himmelsrichtung->grad ( grad->himmelsrichtung ( * 22.5 i ) ) ) )
; )