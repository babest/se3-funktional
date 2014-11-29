#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 05
  
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

; ### Struktur #################################################################
#|

Hinweis: Wenn hier von Eigenschaft und/oder Eigenschaften gesprochen wird, dann 
  sind immer alle 4 'Gene' (Farbe, Musterung, Fühlerform, Flügelform) gemeint.

Gliederung:
1. Bei Aufruf der Hauptfunktion, wir nennen sie mendel, müssen die dominanten
    Eigenschaften von Mutter und Vater übergeben werden, als auch die Anzahl
    der zu 'mendelnen' Kinder.
   Diese Funktion sollte erst mal nur darum kümmern, dass die richtige Anzahl 
    von Kinder gemendelt werden und das eigentliche mendeln weiter delegieren.
   Achtung: Kinder sind nicht immer Zwillinge, also konkrete rezessive Eigenschaft
            nicht vorberechnen.
   [In: dominanten Eigenschaften von Mutter und Vater]
2. Das Zeichen von einem Schmetterling wird von einer weiteren Funktion über-
    nommen, welches die dominanten Eigenschaften von Mutter und Vater übergeben
    bekommt.
   [In: dominanten Eigenschaften von Mutter und Vater]
2.2. Aus jeweils den dominanten Eigenschaften müssen die rezessiv möglichen
      Eigenschaften ermittelt werden unter Beachtung der Vererbungs-/Mendelregeln.
     [In: dominanten Eigenschaften von Mutter und Vater,
     Out: Liste der möglichen rezessiven Eigenschaften von Mutter und Vater]
2.3. Daraus muss dann zufällig eine Eigenschaft ausgewählt werden.
     [In: Liste der möglichen rezessiven Eigenschaften von Mutter und Vater
     Out: Liste von dem rezessiven Gen der jeweiligen Eigenschaften von den Eltern.]
2.4. Aus der dominanten und rezessiven Eigenschaft von jeweils Mutter und Vater
      muss eine Eigenschaft zufällig ausgewählt werden.
     [In: Liste von den dominanten und rezessiven Genen von jeweils Mutter und Vater.
     Out: Liste von den vererbenden Eigenschaften der Eltern]
2.5. Aus den jeweilig zufällig ausgewählt Eigenschaften wird dann die Eigenschaft
      für das Kind ausgewählt - jetzt wieder unter Beachtung der Vererbungsregeln.
      (Die dominante Eigenschaft gewinnt).
     [In: Liste von den vererbenden Eigenschaften der Eltern.
     Out: Liste von dem ausprägenden Eigenschaften des Kindes]
2.6. Mit diesen Informationen kann dann show-butterfly aufgerufen werden.
     [In: Liste von den vererbenden Eigenschaften der Eltern.]

Für die Teilschritte in 2. können neue Funktionen definiert werden oder vielleicht
auch einfach ein Let-Block verwendet werden. Das muss von dem Umfang er einzelnen
Schritte um von In zu Out zu kommen entschieden werden.

Datenstrukturen:
1. Dominanzregeln:
   Hier lässt sich gut eine Liste verwenden, die die Ausprägung eines Gens mit der
   Sortierung von dominant zu rezessiv speichert. Je weiter eine Ausprägung am
   Anfang der Liste, desto dominanter ist es.
-> Mendelregeln:
   Diese Dominanzregeln lassen sich gut wieder in eine Liste verpacken, mit der
   Sortierung: Farbe, Muster, Fühlerform, Flügelform.
   Diese Liste definieren wir als Mendelrules.
2. Schmetterlingseigenschaften
   Eine Liste mit der gleichen Sortierung wie die Mendelregeln, nur mit dem 
   Unterschied das die Elemente keine Listen mit mögliche Eigenschaften sind,
   sondern konkrete Ausprägungen, die dann einfach an show-butterfly übergeben
   werden können.

Begründung:
Das Verpacken der Eigenschaften in Liste ermöglicht und vereinfacht den Umgang
mit einzelnen Eigenschaften UND stellt gleichzeitig die Dominanz der Eigenschaften
in die richtige Reihenfolge.
Das Erstellen einer großen mendelRules Liste vereinfacht den Umgang mit allen
Regeln und vermeidet, dass man jede Eigenschaft einzeln betrachten muss, da so
Funktionen höher Ordnung verwendet werden kann. Allerdings wird das Programm 
dadurch unflexiblerer auf Änderungen bezüglich weiterer Eigenschaften oder einer
Umsortierung der einzelnen Gene reagieren. Dies betrifft nicht die Reihenfolge
der Dominanzregeln. Hier kann einfach die Reihenfolge der Gene geändert, ergänzt
und verringert werden.

|#

; ### Implemantation ###########################################################
(require se3-bib/butterfly-module-2014)

; Demo from task paper:
;(show-butterfly 'red 'stripes 'curved 'rhomb)
;(show-butterfly 'yellow 'stripes 'straight 'hexagon)
;(show-butterfly 'blue 'dots 'curly 'ellipse )


; Grundfunktion
; Gibt aus einer Liste ein zufälliges Element zurück
(define (listPickRandomItem l)
  (list-ref l (random (length l))))

; Grundfunktion
; Liefert aus das Element aus der needleList zurück, welches als erstes 
; in der haystackList gefunden wird oder eben die leere Liste.
; (find-first-of '(a b c) '(c b)) -> 'b
; (find-first-of '(c a b) '(c b)) -> 'c
; (find-first-of '(c a b) '(d)) -> '()
(define (find-first-of haystackList needleList)
  (if
   (empty? haystackList)
   '()
   (if 
    (list? (member (car haystackList) needleList))
    (car haystackList)
    (find-first-of (cdr haystackList) needleList)
   )
  )
)


; Die Definition der Mendelregeln. Dies ist eine Liste mit Unterlisten für 
; die Farbe, die Musterung, die Fühlerform und die Flügelform.
; Je weiter links (am Anfang der Liste), desto dominanter ist das Merkmal
(define mendelRules
  (list
    (list 'blue 'yellow 'red )
    (list 'stripes 'dots 'star )
    (list 'curved 'straight 'curly )
    (list 'hexagon 'rhomb 'ellipse )
  )
)

; Zeichnet zu gegebenen mütterlichen und väterlichen dominaten Eigenschaften
; einen möglichen Kindschmetterling.
; Die Parameter müssen die Form einer Liste haben, bestehend aus den 4 Elementen:
;  Farbe, Muster, Fühlerform, Flügelform
(define (mendel-draw motherDominant fatherDominant)
  (let*
      (
        ; xxxRecessive:
        ;  Es werden die Eigenschaften auf die Mendelregeln gemappt. Somit besteht
        ;  die Ausgabelisten der einzelnen Merkmale nur noch aus möglichen rezessiven
        ;  Eigenschaften, jedoch nicht mehr aus noch dominanteren.
        ;  Danach werden die Eigenschaften noch gemappt und ein Element rausgepickt (Zufall),
        ;  sodass dann eine Liste mit den einzelen rezessiven Eigenschaften übrig bleibt.
        [motherRecessive (map listPickRandomItem (map member motherDominant mendelRules ))]
        [fatherRecessive (map listPickRandomItem (map member fatherDominant mendelRules ))]
        ; xxxRandom:
        ;  Einfach: Die dominante und rezessive Eigenschaft wird zusammengeworfen und dann
        ;  ein jeweils eine Eigenschaft zufällig ausgewählt.
        ;  Genauer: Die beiden Listen mit den dominanten und rezessiven Merkmalen werden
        ;  verbunden (map list) und dann wie eben zufällig ein Element heraus gepickt.
        [motherRandom (map listPickRandomItem (map list motherDominant motherRecessive))]
        [fatherRandom (map listPickRandomItem (map list fatherDominant fatherRecessive))]
        ; child:
        ;  Zuerst werden die Eigenschaften von Mutter und Vater gemappt/verbunden. Daraus
        ;  entsteht wie ebene eine Liste von Paare der Eigenschaften. Hieraus wird nun unter
        ;  Berücksichtigung der Mendelregeln die dominantere ausgewählt. Dies wird über
        ;  find-first-of gelöst, dass das frühere/dominantere Element/Eigenschaft in einer
        ;  Liste findet. Muss für alle Eigenschaften gemacht werden, also map verwenden.
        [child (map find-first-of mendelRules (map list motherRandom fatherRandom))]
      )
    
      ; Dann nur noch anzeigen.
      ; Durch die Struktur von child, können hier jetzt nacheinander die einzelnen
      ; Eigenschaften ausgelesen werden.
      (display (show-butterfly (car child) (cadr child) (caddr child) (cadddr child) ))
    )
)

; Zeichnet n-mal Schmetterlingskinder zu gegebenen mütterlichen und väterlichen dominaten Eigenschaften.
; (Mindestens einer wird gezeichnet)
; Die Parameter müssen die Form einer Liste haben, bestehend aus den 4 Elementen:
;  Farbe, Muster, Fühlerform, Flügelform
(define (mendel motherDominant fatherDominant n)
  (mendel-draw motherDominant fatherDominant)
  (if
   (> n 1)
   (mendel motherDominant fatherDominant (sub1 n))
   (display "\n")
  )
)

; ### Erprobung ################################################################
#|

Auswahl:
Demo 1: Feste Kombination, die immer so angezeigt wird
Demo 2: (Fast) feste Kombination. Es ist zwar alles möglich, jedoch ist auf Grund
        der doppelten maximal dominanten Eigenschaften die Wahrscheinlichkeit sehr
        hoch, dass am Ende wieder das maximal dominante herauskommt.
Demo 3: Ein bisschen mehr gemixt (maximale und durchschnittliche Dominanz), aber
        komplett rezessive Ausprägungen sind unwahrscheinlich.
Demo 4: Wie eben, nur das jetzt die maximal dominanten Eigenschaften NICHT möglich
        sind.

Durch diese Auswahl kann überprüft werden, ob die Vererbungsregeln richtig
implementiert worden sind. Außerdem lässt sich damit auch sehen, wie wahrscheinlich
einige Kinder-Kombinationen sind, bei zwei (unterschiedlichen) Elternteilen.

|#


; Constant to set the amount of butterflies, which should be draw by each demo call
(define NUM_OF_BUTTERFLIES_TO_DRAW 3)

(display "Just this recessive combination -> red, star, curly, ellipse:\n")
(mendel 
 (list 'red 'star 'curly 'ellipse ) 
 (list 'red 'star 'curly 'ellipse ) 
 2
)

(display "Everything is possible, but blue, stripes, curved, hexagon is probable:\n")
(mendel 
 (list 'blue 'stripes 'curved 'hexagon ) 
 (list 'blue 'stripes 'curved 'hexagon )
 NUM_OF_BUTTERFLIES_TO_DRAW
)

(display "Everything is still possible (more mixed):\n")
(mendel 
 (list 'blue 'stripes 'curved 'hexagon ) 
 (list 'red 'star 'curly 'ellipse ) 
 NUM_OF_BUTTERFLIES_TO_DRAW
)

(display "Much is possible, but not blue, stripes, curved, hexagon:\n")
(mendel 
 (list 'red 'star 'curly 'ellipse ) 
 (list 'yellow 'dots 'straight 'rhomb )
 NUM_OF_BUTTERFLIES_TO_DRAW
)