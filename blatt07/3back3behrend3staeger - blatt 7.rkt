#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 07
  
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#



; ##############################################################################
; ## Imports ###################################################################
; ##############################################################################

(require 2htdp/image);



; ##############################################################################
; ## Hilfsfunktionen ###########################################################
; ##############################################################################

; Berechnet die Differenz einer Range, bzw eines Werte-Paars.
; Dabei wird das erste Element vom zweiten abgezogen.
(define (diff range)
  ( - (cdr range) (car range) )
  )

; Gibt das letzte Element einer Liste zurück
(define (last-element list)
  ( car (reverse list) )
  )



; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################

; Range allgemein rekursiv
(define (range1 interval n)
  (define (helper step)
    ( if (= step n)
         '()
         ( cons
           ( + ( * ( / (diff interval) n ) step ) (car interval))
           ( helper (add1 step) )
           )
         )
    )
  ( helper 0 )
  )

; Beispielaufruf
;( range1 '(10 . 20) 5 )

; Range endrekursiv
(define (range2 interval n)
  (define (helper step acc)
    ( if (= step n)
         ( reverse acc )
         ( helper
           (add1 step)
           ( cons
             ( + ( * ( / (diff interval) n ) step ) (car interval))
             acc
             )
           )
         )
    )
  ( helper 0 '() )
  )

; Beispielaufruf
;( range2 '(10 . 20) 5 )

; Range mit FHOs
(define (range3 interval n)
  ( build-list
    n
    ( lambda (step)
       ( + ( * ( / (diff interval) n ) step ) (car interval))
       )
    )
  )

; Beispielaufruf
;( range3 '(10 . 20) 5 )



; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

; Berechnet zu einem Interval n Werte-Paare für die Funktion func
(define (function->points func interval n)
  ( map cons (range1 interval n) (map func (range1 interval n)) )
  )

; Beispielaufruf
;( function->points sqr '(0 . 10) 5 )



; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################

; Scaliert eine Range auf ein neues Interval
(define (rescale1d source-range destination-interval)
  (let*
      ( ; Im ersten Schritt normalisieren wir die Liste, dass sie mit 0 anfängt
       ; Also von jedem Element den Wert des ersten Elements abziehen
       ; '(2 4 6) -> '(0 2 4) 
       [ list-zero ( map (curryr - (car source-range)) source-range ) ]
       ; Im zweiten Schritt normalisieren wir die Liste,
       ; so dass sie nur Werte von 0 bis 1 enthält
       ; '(0 2 4) -> '(0 1/2 1)
       [ list-norm ( map (curryr / (last-element list-zero)) list-zero ) ]
       ; Im dritten Schritt multiplizieren wir alle Elemente der normierten
       ; Liste mit der Different des destination-interval
       [ list-scaled ( map (curry * (diff destination-interval)) list-norm ) ]
       ; Jetzt müssen wir nur noch zu jedem Element die untere Grenze des
       ; destination-interval hinzuzählen
       [ list-pushed ( map (curry + (car destination-interval)) list-scaled ) ]
       )
    list-pushed
    )
  )

; Beispielaufruf
;( rescale1d '(0 2 4 6 8) '(10 . 50) )


; Scaliert eine Menge von zweier-Tupeln auf zwei neue Intervalle
(define (rescale2d source dest-interval1 dest-interval2)
  (let*
      (
       [ first-scaled (rescale1d ( map car source ) dest-interval1) ]
       [ second-scaled (rescale1d ( map cdr source ) dest-interval2) ]
       )
    ( map cons first-scaled second-scaled )
    )
  )

; Beispielaufruf
;( rescale2d '( ( 0 . 0) (2 . 4) (4 . 16) (6 . 36) (8 . 64) ) '(10 . 50) '(5 . 25) )



; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################

; Größe des Bildes
( define scene-width 800 )
( define scene-height 600 )

; Dieses Rechteck ist auf dem Bild am Ende nicht sichtbar, wird jedoch verwendet
; um die einzelnen Objekte auf dem Bild zu platzieren.
( define orientation-canvas ( rectangle scene-width scene-height "solid" ( color 0 0 0 0 ) ) )


(define (draw-points pointlist)
  (let*
      ( [ point (place-image (circle 1 "solid" "blue") (caar pointlist) (- scene-height (cdar pointlist)) orientation-canvas) ] )
    ( cond 
       [ (empty? pointlist) ]
       [ (= (length pointlist) 1) point ]
       [ else ( underlay
                point
                ( draw-points (cdr pointlist) )
                ) ]
       )
    )
  )

; Beispielaufruf
;(draw-points (function->points sqr '(0 . 50) 200))
;(draw-points (function->points (curry * 0.5) '(0 . 800) 300))

