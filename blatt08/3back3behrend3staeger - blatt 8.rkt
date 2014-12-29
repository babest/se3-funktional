#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 08
  
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#



; ##############################################################################
; ## TODO ######################################################################
; ##############################################################################
#|
- Aufgabe 1.3 überprüfen
- Aufgabe 3 SET
- Aufgabe 3.2: geht das Erzeugen des Decks auch cooler?
|#



; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################

#|

1.
Eine Funktion höherer Ordnung zeichnet sich dadurch aus, dass sie entweder
eine oder mehrere Funktion(en) als Parameter übergeben bekommt und/oder eine
Funktion als Ergebnis liefert.

2.
(a) Bei foldl handelt es sich um eine FHO, da eine Funktion als Parameter
    erwartet wird.
(b) Bei dieser Funktion handelt es sich um eine FHO, da der Rückgabewert
    der Funktion eine Funktion ist (entweder car oder cdr)
(c) Die Funktion ist eine FHO, die die gleiche Funktionalität, wie curry
    bietet. Sowohl der erster Parameter, als auch der Rückgabewert ist
    eine Funktion
(d) my-tan ist keine FHO, da weder eine Funktion als Parameter erwartet wird,
    noch eine Funktion als Rückgabewert vorliegt.

3.
((pimento * 5) 7)
Bei der Funktion pimento gibt es zwei Umgebungen. Die erste Umgebung ist der
Funktionsaufruf der Funktion pimento, also: (pimento * 5). Die zweite Umgebung
ist der Aufruf der Funktion, die pimento zurückgibt, also (procedure 7).
Die Funktion piemento gibt also aus ihrem Aufruf eine Funktion zurück, die in
der zweiten Umgebung verwendet wird. In der ersten Umgebung ist dabei sowohl
die Funktion * und der Wert 7 bekannt, sie werden beim Aufruf der Funktion
piemento gebunden und es wird ein closure erzeugt. Die Funktion / der Closure,
die von piemento also erzeugt wird, hat die Funktion * und den Wert 7 gebunden,
von "außen" kann jedoch auf diese Werte nicht zugegriffen werden.
In der zweiten Umgebung wird dann der Wert 7 an den Closure gebunden und aus-
gewertet.

4.

(foldl (curry * 2) 1 '(1 1 2 3))
-> 96
Als erstes wird eine Funktion erzeugt, die das Produkt aller Eingabewerte verdoppelt.
Danach werden in die Funktion der Startwert und das erste Element der Liste eingereicht:
((curry * 2)  1 1) ->  2
Jetzt wird in die selbe Funktion das Ergebnis und der zweite Wert der Liste eingereicht:
((curry * 2)  2 1) ->  4
usw...
((curry * 2)  4 2) -> 16
((curry * 2) 16 3) -> 96

(map cons '(1 2 3 4) '(1 2 3 4))
-> '((1 . 1) (2 . 2) (3 . 3) (4 . 4))
Die Funktion map wendet eine übergebene Funktion (in unserem Fall cons) auf beliebig
viele Listen an, wobei immer das i-te Element aus jeder Liste verwendet wird, und gibt
das Ergebnis in einer Liste zurück.
Es wird also zuerst (cons 1 1), dann (cons 2 2) usw. aufgerufen.

( filter pair? '((a b) () 1 (()) ) )
-> '((a b) (()))
Die Funktion Filter gibt eine Liste zurück, die aus den Elementen einer übergebene Liste
besteht, die mit der übergebenen Funktion als #t ausgewertet wurden.
(pair? '(a b)) -> #t
(pair? '())    -> #f
(pair? 1)      -> #f
(pair? '(()))  -> #t

( map ( compose ( curry + 33) ( curry * 1.8 ) ) '(5505 100 0 1 1000 -273.15) )
-> '(9942.0 213.0 33 34.8 1833.0 -458.669...)
Die Funktion compose erzeugt eine Funktion, die die übergebenen Funktionen nacheinander
ausführt, die letzte Funktion als erstes, die vorderste als letztes. Das Ergebnis ist
in diesem Fall also:
( lambda (x) ( + 33 (* 1.8 x) ) )
Diese Funktion wird mit map auf jedes Element der Liste angewendet. Also jede Zahl mit
1.8 multiplizieren und zu dem Ergebnis 33 addieren.

|#



; ##############################################################################
; ## Aufgabe 2 #################################################################
; ##############################################################################

( define xs '(1 2 -3 4 5 12) )

; Gibt eine Liste mit Beträgen zurück
( define (betraege list)
   ( map abs list)
   )

;(betraege xs)

; Gibt eine Liste aller Elemente zurück, die glatt durch drei teilbar sind
( define (durchdreiteilbar list)
   ( filter ( compose zero? ( curryr remainder 3)) list )
   )

;(durchdreiteilbar xs)

; Gibt die Summe aller Elemente zurück, die größer als 10 sind
( define (summegrosserzaheln list)
   ( foldl + 0 ( filter ( curry < 10 ) list ) )
   )

;(summegrosserzaheln xs)

; Teilt eine Liste in zwei Teilliste mit nur geraden und ungeraden Elementen.
(define (splitEvenOdd numList)
  (partition even? numList)
  )

;(splitEvenOdd xs)



; ##############################################################################
; ## Aufgabe 3.1 ###############################################################
; ##############################################################################

#|
Die Repräsentation der möglichen Eigenschaften einer Karte implementieren wir
als vier verscheidene Listen, die jeweils alle möglichen Ausprägung einer
Eigenschaft enthalten.
|#
( define counts '(1 2 3) )
( define patterns '(waves oval rectangle) )
( define modes '(outline solid hatched) )
( define colors '(red green blue) )

#|
Eine einzelne Karte implementieren wir ebenfalls als eine List, die ihre Eigen-
schaften in folgender Reihenfolge enthält: Anzahl, Form, Füllmuster, Farbe.
|#
( define sample-card '(2 waves hatched green) )



; ##############################################################################
; ## Aufgabe 3.2 ###############################################################
; ##############################################################################

(require "setkarten-module.rkt")

; Hilfsfunktion
; Zeigt eine Set-Karte an mit der angegbeben Funktion
( define ( show-card card )
   ( apply show-set-card card )
   )

; Erzeugt ein Deck mit 81 Set Karten, bei dem eine Karte jeder möglichen
; Kombination vorhanden ist
( define ( create-deck )
   (for*/list ( [i counts]
                [j patterns]
                [k modes]
                [l colors]
                )
     (append (list i j k l))
     )
   )

; Gibt eine Liste von Karten aus
( define ( visualize-cards cards )
   ( map show-card cards )
   )



; ##############################################################################
; ## Aufgabe 3.3 ###############################################################
; ##############################################################################

