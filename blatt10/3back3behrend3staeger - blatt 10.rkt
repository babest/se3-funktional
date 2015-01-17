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
   ( define ( helper list )
      ( cond
         [ ( empty? list ) #f ]
         [ ( p? (car list) ) ( car list ) ]
         [ else ( helper (cdr list) ) ]
         )
      )
   ( helper xs )
   )

; Beispielaufruf
;(some (curry = 3) '(1 3 9))



; ##############################################################################
; ## Aufgabe 1.2 ###############################################################
; ##############################################################################

; Testdaten

( define symmetrisch  '((1 . 3) (2 . 2) (3 . 1)) )
( define asymmetrisch '((1 . 2) (1 . 3) (2 . 3)) )
( define reflexiv '((1 . 1) (1 . 3) (3 . 3)) )
( define transitive '((1 . 2) (2 . 3) (1 . 3)) )
( define notTransitive '((1 . 2) (2 . 3) (1 . 3) (2 . 4)) )
( define aequiR '((1 . 1) (3 . 3) (1 . 3) (3 . 1)) )
( define ordnung asymmetrisch)


; Hilfsmethode
; Prüft, ob ein Element in einer Liste vorhanden ist und gibt #t oder #f zurück
( define ( contains? element list )
   ( if
     ( some (curry equal? element) list )
     #t
     #f
     )
   )

; Hilfsmethode
; Prüft, ob ein Element nicht einer Liste vorhanden ist und gibt #t oder #f zurück
( define ( notcontains? element list )
   ( not
     ( contains? element list )
     )
   )

; Prüft, ob eine übergebene Relation symmetrisch ist
( define ( symmetrisch? r )
   ( every
     ; Für jedes Paar aus der Liste, muss es in der Liste auch ein Paar geben,
     ; bei dem die Einträge genau anders rum sind.
     ( λ (x) ( contains? (cons (cdr x) (car x)) r ) )
     r
     )
   )

; Prüft ob eine übergebene Relation asymmetrisch ist
( define ( asymmetrisch? r )
   ( every
     ; Für jedes Paar aus der Liste, darf es in der Liste kein Paar geben,
     ; bei dem die Einträge genau anders rum sind.
     ( λ (x) ( notcontains? (cons (cdr x) (car x)) r ) )
     r
     )
   )

; Prüft ob eine übergebene Relation reflexiv ist
( define ( reflexiv? r )
   ( let (
          ; Vorher müssen wir wissen, welche Elemente überhaupt in der Relation vorkommen
          [ elements ( remove-duplicates (flatten r) ) ]
          )
      ; Für jedes vorkommende Element muss es ein Paar geben, dass an beiden
      ; Stellen das Element stehen hat.
      ( every
        ( λ (x) ( contains? (cons x x) r ) )
        elements
        )
      )
   )

; Prüpft ob eine übergebene Relation transitiv ist
( define ( transitiv? r )
   (let
       ([rNoReflexiv (filter ( λ (x) (not (= (car x) (cdr x)))) r)])
     ; Überprüpft die Transitivität für einen Knoten
     ( define ( edgeTester startNode )
        ;(displayln startNode)
        ;Überprüpft, die Transitivität zwischen Knoten (startKnoten und nextKnoten)
        ( define ( edgePathTester nextNode )
           ;(display "Teste start: ")
           ;(display startNode)
           ;(display ", next: ")
           ;(displayln nextNode)
           (if
            ; Testet auf einen (Teil-)Zyklus (Zyklus -> transitiv)
            (eqv? (cdr nextNode) (car startNode))
            #t
            (if
             ; Existiert ein Pfad zwischem den (einzelnen) Startknoten und diesem Knoten?
             (contains? (cons (car startNode) (cdr nextNode)) rNoReflexiv)
             ; Überprüpft alle ausgehenden Pfade und prüpft einen Pfad
             (every edgePathTester (filter ( λ (x) (= (car x) (cdr nextNode))) rNoReflexiv))
             #f
             )
            )
           )
        ; Überprüpft ob ein Pfad zwischen den Startknoten und allen ausgehenden Knoten des Startknoten existiert
        (every edgePathTester (filter ( λ (x) (= (car x) (cdr startNode))) rNoReflexiv))
        )
     ; Überprüpft die Transitivität für jeden Knoten
     (every edgeTester rNoReflexiv)
     )
)

; Überprüpft ob eine Relation eine Äquivalenzrelation ist
; Eine Äquivalenzrelation hat folgende Eigenschaften: reflexiv, symmetrisch und transitiv
( define (aequi? r)
   (and
    (reflexiv? r)
    (symmetrisch? r)
    (transitiv? r)
    )
   )

; Überprüpft ob eine Relation eine strikte Ordnung ist
; Eine Äquivalenzrelation hat folgende Eigenschaften: asymmetrisch und transitiv
(define (ord? r)
  (and
   (asymmetrisch? r)
   (transitiv? r)
   )
  )


;(symmetrisch? symmetrisch) ; -> #t
;(symmetrisch? asymmetrisch) ; -> #f

;(asymmetrisch? asymmetrisch) ; -> #t
;(asymmetrisch? symmetrisch) ; -> #f

;(reflexiv? reflexiv) ; -> #t
;(reflexiv? symmetrisch) ; -> #f

;(transitiv? transitive) ; -> #t
;(transitiv? notTransitive) ; -> #f

;(aequi? aequiR) ; -> #t
;(aequi? notTransitive) ; -> #f

;(ord? ordnung) ; -> #t
;(ord? notTransitive) ; -> #f


; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################