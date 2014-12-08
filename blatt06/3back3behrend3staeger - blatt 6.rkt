#lang racket

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

# kopfstueck
- lineare Rekursion, da kopfstück sich immer maximal nur ein mal aufruft
- direkte Rekursion, da keine weitere Funktion verwendet wird, die sich wechselseitig aufrufen


# endstueck
- lineare Rekursion, da endstück sich immer maximal nur ein mal aufruft
- direkte Rekursion, da keine weitere Funktion verwendet wird, die sich wechselseitig aufrufen
- End-Rekursion, da das Ergebnis nicht mit anderen Termen verknüpft wird

# merge
- lineare Rekursion, da merge sich immer maximal nur ein mal aufruft
- direkte Rekursion, da keine weitere Funktion verwendet wird, die sich wechselseitig aufrufen

# merge-sort
- Baumrekursion, da sich die Funktion immer zweimal selber aufruft
- direkte Rekursion, es werden zwar andere Funktionen verwendet, diese Funktionen rufen aber nicht
  wieder merge-sort auf, somit ist keine Wechselseitigkeit vorhanden


# Hilfe

## Definitionen 

Linear-Rekursiv: Eine Funktionsdefinition, die sich auf der rechten Seite der
definierenden Gleichung in jeder Fallunterscheidung selbst nur einmal verwendet.

Baumrekursiv: Eine rekursive Definition ist baumartig wenn in der Definition in
einer Fallunterscheidung mehrfach auf die Definition Bezug genommen wird.

Geschachtelt: Eine Rekursion ist geschachtelt, wenn die Funktion in der rekursiven
Verwendung selbst als Argument mitgegeben wird.

Indirekt: Eine rekursive Definiton heißt indirekt oder verschränkt, wenn zwei oder
mehrere Definitionen sich wechselseitig rekursiv verwenden.

Endrekursiv: Rekursive Funktionen, bei denen das Ergebnis der Rekursion nicht mehr
mit anderen Termen verknüpft werden muß.

## Die möglichen Antworten warne

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
           ( underlay ; Sterne übereinander legen
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
              ; Verwendung einer Exponentialfunktion, damit
              ; die Elemente immer kleiner werden
              [ fak ( expt (/ 2 3) step ) ]
              [ new-size ( * fak init-size ) ]
              )
         ; Bevor die Elemente zu klein werden, abbbrechen
         ( if ( < fak  0.1 )
              ( triangle new-size "solid" "darkgreen" ) ; Oberste Baumspitze
              
              ; Unteranderlegen von...
              ( underlay/align/offset
                "center"
                "middle"
                ( triangle new-size "solid" "black" ) ; ... dem dünnem schwarzem Rahmen ...
                0
                (* new-size -0.2)
              
                ( underlay/align/offset
                  "center"
                  "middle"
                  ( triangle new-size "solid" "darkgreen" ) ; ... und dem Baumelementstück ...
                  0
                  (* new-size -0.4)
                  ( helper init-size (add1 step) ) ; ... und dem nächsten Stück
                  )
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
           ( underlay ; Bäume übereinander legen
             tree
             ( trees (sub1 n) )
             )
           )
      )
   )

; Generiert 10 Bäume
( define ground-full-of-trees
   ( trees 8 )
   )


; Generiert einen Pythagoras Baum mit 5 Stufen
(define (one-pythagoras-tree)
  ( define ( helper size step )
      ( let* (
              [rect ( square size "solid" "yellowgreen" )]
              [rect-new-size (/ (sqrt (* 2 (expt size 2)))2) ] ; Mathematik für die neue Seitenlänge a^2 = b^2 + c^2
              )
         (if (>= step 4) ; not more than four. Otherwise will the above/align function break and the tree doesnt look good
             rect
             ( above/align
                "center"
                (beside
                 (rotate 045 ( helper rect-new-size (add1 step) ))
                 (rotate 315 ( helper rect-new-size (add1 step) ))
                )
                rect
             ) 
          )
         )
      )
   ( helper 20 0 )
)

; Generiert auf dem orientation-canvas n Pythagorasbäume an zufälligen Positionen und gibt ein
; Image zurück, auf dem die Pythagorasbäume (übereinander) liegen.
( define (pythagoras-trees n)
   ( let (
          [ pythagoras-tree ( place-image
                   (one-pythagoras-tree)
                   (random scene-width)
                   (+ (random (inexact->exact (* scene-height 0.30))) (inexact->exact (* scene-height 0.60)))
                   orientation-canvas ) ]
          )
      ( if (zero? n)
           pythagoras-tree
           ( underlay ; Pythagorasbäume übereinander legen
             pythagoras-tree
             ( pythagoras-trees (sub1 n) )
             )
           )
      )
   )

; Generiert das Pythagorasbäume-Image
( define ground-full-of-pythagoras-trees
   ( pythagoras-trees 3 )
)




; Zeigt das Bild
( define ( create-scene )
   ( underlay
     my-scene
     sky-full-of-stars
     snow-landscape
     ground-full-of-pythagoras-trees
     ground-full-of-trees
     )
   )

; Testaufruf
(create-scene)

