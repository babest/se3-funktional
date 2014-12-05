#lang racket

;;; TODO: Erklärung Aufgabe 1!!!
;;; TODO: Baumrekursion (nein, nicht mehrere Bäume zeichnen :))!!!

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 06
  
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#



; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################

#|

Eine Funktionsdefinition, die sich auf der rechten Seite der definierenden Gleichung
in jeder Fallunterscheidung selbst nur einmal verwendet, heißt linear-rekursiv.

Eine rekursive Definition ist baumartig wenn in der Definition in einer
Fallunterscheidung mehrfach auf die Definition Bezug genommen wird.

Eine Rekursion ist geschachtelt, wenn die Funktion in der rekursiven Verwendung
selbst als Argument mitgegeben wird.

Eine rekursive Definiton heißt indirekt oder verschränkt, wenn zwei oder mehrere
Definitionen sich wechselseitig rekursiv verwenden.

Rekursive Funktionen, bei denen das Ergebnis der Rekursion nicht mehr mit anderen
Termen verknüpft werden muß, heißen endrekursiv.


# kopfstueck
- lineare Rekursion
- direkte Rekursion

# endstueck
- lineare Rekursion
- direkte Rekursion
- End-Rekursion

# merge
- lineare Rekursion
- direkte Rekursion

# merge-sort
- Baumrekursion
- direkte Rekursion


# mögliche antworten
- lineare Rekursion
- Baumrekursion
- geschachtelte Rekursion
- direkte Rekursion
- indirekte Rekursion
- End-Rekursion

|#



; ##############################################################################
; ## Aufgabe 2 #################################################################
; ##############################################################################

(require 2htdp/universe)
(require 2htdp/image)

; Der Baum vom Übungsblatt
( define baum1
   ( above/align
     "center"
     ; der Stern an der Spitze
     ( star-polygon 40 5 2 "solid" "gold" )
     ; die Zweige
     ( ellipse 20 40 "solid" "darkgreen" )
     ( ellipse 80 50 "solid" "darkolivegreen" )
     ( ellipse 130 60 "solid" "olivedrab" )
     ( ellipse 180 80 "solid" "darkgreen" )
     ; der Stamm
     ( rectangle 40 60 "solid" "brown" )
     )
   )

; Größe des Bildes
( define scene-width 800 )
( define scene-height 600 )

; Diese Szene ist der Hintergrund des Bildes
( define my-scene ( empty-scene scene-width scene-height "darkblue" ) )

; Dieses Rechteck ist auf dem Bild am Ende nicht sichtbar, wird jedoch verwendet
; um die einzelnen Objekte auf dem Bild zu platzieren.
( define orientation-canvas ( rectangle scene-width scene-height "solid" ( color 0 0 0 0 ) ) )

; Hier wird eine Schneelandschaft erzeugt, die aus mehreren Ellipsen besteht,
; die mit unterschiedlicher Größe und Schattierung übereinander liegen. *magic*
( define snow-landscape
   ( underlay
     ( place-image (ellipse 1200 500 "solid" (color 180 180 180)) 200 600 orientation-canvas )
     ( place-image (ellipse 1000 700 "solid" (color 220 220 220)) 600 700 orientation-canvas )
     ( place-image (ellipse 1000 600 "solid" (color 240 240 240)) 200 700 orientation-canvas )
     ( place-image (ellipse 2000 600 "solid" (color 250 250 250)) 400 800 orientation-canvas )
     )
   )

; Generiert einen Stern, mit einer zufälligen Größe und einer zufälligen Gelb-Schattierung
( define ( onestar )
   ( star-polygon
     ( + (random 5) 1 )
     9
     4
     "solid"
     ( color 255 255 (+ (random 80) 80 ) )
     )
   )

; Generiert auf dem orientation-canvas n Sterne an zufälligen Positionen und gibt ein
; Image zurück, auf dem die Sterne übereinander liegen.
( define ( stars n )
   ( let (
          [ star ( place-image
                   (onestar)
                   (random scene-width)
                   (random (inexact->exact (* scene-height 0.60)))
                   orientation-canvas ) ]
          )
      ( if (zero? n)
           star
           ( underlay
             star
             ( stars (sub1 n) )
             )
           )
      )
   )

; Generiert 100 Sterne
( define sky-full-of-stars
   ( stars 100 )
   )

; Generiert einen Baum mit einer zufälligen Größe
( define ( onetree )
   ( define ( helper init-size step )
      ( let* (
              [ fak ( expt (/ 2 3) step ) ]
              [ new-size ( * fak init-size ) ]
              )
         ( if ( < fak  0.1 )
              ( triangle new-size "solid" "darkgreen" )
              ( underlay/align/offset
                "center"
                "middle"
                ( triangle new-size "solid" "darkgreen" )
                0
                (* new-size -0.4)
                ( helper init-size (add1 step) )
                )
              )
         )
      )
   ( helper (+ (random 50) 50) 0 )
   )

; Generiert auf dem orientation-canvas n Bäume an zufälligen Positionen und gibt ein
; Image zurück, auf dem diese übereinander liegen.
( define (trees n)
   ( let (
          [ tree ( place-image
                   (onetree)
                   (random scene-width)
                   (+ (random (inexact->exact (* scene-height 0.30))) (inexact->exact (* scene-height 0.60)))
                   orientation-canvas ) ]
          )
      ( if (zero? n)
           tree
           ( underlay
             tree
             ( trees (sub1 n) )
             )
           )
      )
   )

; Generiert 10 Bäume
( define ground-full-of-trees
   ( trees 10 )
   )



; Zeigt das Bild
( define ( create-scene )
   ( underlay
     my-scene
     sky-full-of-stars
     snow-landscape
     ground-full-of-trees
     )
   )

(create-scene)

