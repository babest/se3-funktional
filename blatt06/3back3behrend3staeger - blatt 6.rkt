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