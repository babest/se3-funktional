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


; KOMMENTAR: Sehr schön. 40 / 40


; ##############################################################################
; ## Imports ###################################################################
; ##############################################################################

(require 2htdp/image) ; Image

(require 2htdp/universe) ; Animate

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
      ( ; Im ersten Schritt normalisieren wir die Liste, dass das kleinste
       ; Element der Liste die 0 ist. Anders gesagt: von jedem Element der Liste
       ; den Wert des kleinsten Elements aus der Liste abziehen.
       ; '(2 4 6) -> '(0 2 4) und '(0 1 0 -1) -> '(1 2 1 0)
       [ list-zero ( map (curryr - (argmin (curry + 0) source-range)) source-range ) ]
       ; Im zweiten Schritt normalisieren wir die Liste, so dass sie nur Werte von
       ; 0 bis 1 enthält. Anders gesagt, wir teilen jedes Element der Liste
       ; durch den Wert des größten Elements der Liste
       ; '(0 2 4) -> '(0 1/2 1) und '(1 2 1 0) -> '(1/2 1 1/2 0)
       [ list-norm ( map (curryr / (argmax (curry + 0) list-zero)) list-zero ) ]
       ; Im dritten Schritt multiplizieren wir alle Elemente der normierten
       ; Liste mit der Different des destination-interval.
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
;( rescale1d '(0 1 2 1 0 -1 -2 -1 0) '(0 . 4) )


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
;( rescale2d '( ( 0 . 0) (2 . 1) (4 . 0) (6 . -1) (8 . 0) (10 . 1) ) '(0 . 10) '(0 . 2) )



; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################

; Größe des Bildes
( define scene-width 800 )
( define scene-height 600 )

; Dieses Rechteck ist auf dem Bild am Ende nicht sichtbar, wird jedoch verwendet
; um die einzelnen Objekte auf dem Bild zu platzieren.
( define orientation-canvas ( rectangle scene-width scene-height "solid" ( color 0 0 0 0 ) ) )

; Zeichnet eine Liste von Punkten (x-y-Paar) auf die Zeichenfläche
(define (draw-points pointlist)
  (let*
      ; Den Punkt berechnen mit seiner Position (x: erstes Element vom ersten Tupel der Liste...)
      ( [ point (place-image (circle 1 "solid" "blue") (caar pointlist) (- scene-height (cdar pointlist)) orientation-canvas) ] )
    ( cond
       ; Wenn nur noch ein Punkt in der Liste ist...
       [ (= (length pointlist) 1) point ]
       [ else ( underlay
                point
                ( draw-points (cdr pointlist) )
                ) ]
       )
    )
  )

; Beispielaufruf
;( draw-points (function->points sqr '(0 . 50) 200) )
;( draw-points (function->points (curry * 0.5) '(0 . 800) 300) )



; ##############################################################################
; ## Aufgabe 2.6 # Zusatzaufgabe ###############################################
; ##############################################################################

; Erstellt ein Bild mit Linien, die den Verlauf der Punkte zeigt.
; Dafür muss pointlist aus mindestens 2 Punkten bestehen
(define (draw-lines pointlist)
  (let*
      ; Die berechnen mit seiner Position
      ( [ line (place-image 
                (line 
                 (- (caar pointlist) (caadr pointlist)) ; Linien x-Richtung/Vektor
                 (- (cdadr pointlist) (cdar pointlist)) ; Linien y-Richtung/Vektor
                 "blue"
                 ) 
                (caar pointlist) 
                (- scene-height (cdar pointlist)) 
                orientation-canvas) 
               ] 
        )
    ( cond
       ; Wenn noch weitere (>= 2) Punkte vorhanden sind, dann die weiteren Linien auch malen
       [ (= (length pointlist) 2) line ]
       [ else ( underlay
                line
                ( draw-lines (cdr pointlist) )
                ) ]
       )
    )
  )

; Beispielaufruf
;( draw-lines (function->points sqr '(0 . 50) 200) )
;( draw-lines (function->points (curry * 0.5) '(0 . 800) 300) )



; ##############################################################################
; ## Aufgabe 2.4 ###############################################################
; ##############################################################################

; Zeichnet auf einen Graph n-Viele Punkte im Interval interval erzeugt durch die Funktion func
(define (plot-function func interval n lines-instead-points?)
  (let*
      (
       ; Errechnet alle Punkte (genau wie plot-function )
       [allPoints ( rescale2d
                    ( function->points func interval n )
                    ( cons 0 scene-width)
                    ( cons 0 scene-height)
                    )]
       )
    ; Unterscheiden nach Punkt- oder Liniendarstellung
    (if lines-instead-points?
        ( draw-lines allPoints)
        ( draw-points allPoints)
        )
    )
  )

; Beispielaufruf
;( plot-function sin (cons 0 (* 2 pi)) 200 #f)
;( plot-function cos (cons 0 (* 2 pi)) 200 #t)
;( plot-function tan '(-1.5 . 1.5) 200 #f)
;( plot-function sqr '(-10 . 10) 200 #t)
;( plot-function sqrt '(0 . 10) 200 #t)


; ##############################################################################
; ## Aufgabe 2.4 ###############################################################
; ##############################################################################

; Zeichnet auf einen Graph n-Viele Punkte im Interval interval erzeugt durch die Funktion func
; Als letzter Parameter wird der Schritt t bzw. der wievielte Punkt markiert werden soll.
(define (live-plot-function func interval n lines-instead-points? t)
  (let*
      (
       ; Errechnet alle Punkte (genau wie plot-function )
       [allPoints ( rescale2d
                    ( function->points func interval n )
                    ( cons 0 scene-width)
                    ( cons 0 scene-height)
                    )]
       ; Die genaue Nummer des Punkts der hervorgehoben werden soll
       [marker-index (modulo t n)]
       ; Die Koordinaten des Markerpunkts
       [marker (list-ref allPoints marker-index)]
       )
    ; Den Plot zeichnen
    ( underlay
      ; Den Markler zeichnen
      (place-image (ellipse 8 8 "outline" "red") (car marker) (- scene-height (cdr marker)) orientation-canvas)
      
      ; Den Graphen zeichen (genau wie plot-function )
      ; Unterscheiden nach Punkt- oder Liniendarstellung
      (if lines-instead-points?
          (draw-lines allPoints)
          (draw-points allPoints)
          )
      )
    )
  )

; Beispielaufruf
;(animate (curry live-plot-function sin (cons 0 (* 2 pi)) 200 #t))
;(animate (curry live-plot-function cos (cons 0 (* 4 pi)) 200 #f))
