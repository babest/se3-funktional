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

; TODO: Erklärung der Struktur



(require se3-bib/butterfly-module-2014)

;(show-butterfly 'red 'stripes 'curved 'rhomb)
;(show-butterfly 'yellow 'stripes 'straight 'hexagon)
;(show-butterfly 'blue 'dots 'curly 'ellipse )

; Grundfunktion
; Gibt aus einer Liste ein zufälliges Element zurück
(define (listPickRandomItem l)
  (list-ref l (random (length l))))

; Die Definition der Mendelregeln. Dies ist eine Liste mit Unterlisten für 
; die Farbe, die Musterung, die Fühlerform und die Flügelform.
; Je weiter links (am Anfang der Liste), desto dominanter ist das Merkmal
(define mendelRules
  (list
    (list 'blue 'yellow 'red )
    (list 'stripes 'dots 'star )
    (list 'curved 'straight 'curly )
    (list 'hexagon 'ellipse 'rhomb )
  )
)

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

; Zeichnet n (anzahl) Schmetterlingskinder zu gegebenen mütterlichen und väterlichen dominaten Eigenschaften.
; (Mindestens einer wird gezeichnet)
; Die Parameter müssen die Form einer Liste haben, bestehend aus den 4 Elementen:
;  Farbe, Muster, Fühlerform, Flügelform
(define (mendel motherDominant fatherDominant anzahl)
  (mendel-draw motherDominant fatherDominant)
  (if
   (> anzahl 1)
   (mendel motherDominant fatherDominant (sub1 anzahl))
   "Done"
  )
)


(mendel 
 (list 'blue 'stripes 'curved 'hexagon ) 
 (list 'yellow 'dots 'straight 'rhomb ) 
 2
)
(mendel 
 (list 'yellow 'star 'straight 'rhomb ) 
 (list 'yellow 'dots 'curly 'rhomb ) 
 4
)

(mendel 
 (list 'red 'dots 'straight 'rhomb ) 
 (list 'yellow 'dots 'straight 'ellipse ) 
 4
)