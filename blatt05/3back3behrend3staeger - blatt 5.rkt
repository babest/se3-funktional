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

(require se3-bib/butterfly-module-2014)

;(show-butterfly 'red 'stripes 'curved 'rhomb)
;(show-butterfly 'yellow 'stripes 'straight 'hexagon)
;(show-butterfly 'blue 'dots 'curly 'ellipse )


(define (listPickRandomItem l)
  (list-ref l (random (length l))))

(define mendelRegeln
  (list
    '( blue yellow red)
    '( stripes dots star )
    '( curved straight curly )
    '( hexagon ellipse rhomb )
  )
)

(define (dominant->recessiveList ruleList dominant)
  (if 
   (empty? ruleList)
   '()
   (if 
    (eq? (car ruleList) dominant)
    ruleList
    (dominant->recessiveList (cdr ruleList) dominant)
    )
   )
)

(define (getRandomPropertiesOfParent ruleListList dominantList)
  (if 
      (or (empty? ruleListList) (empty? dominantList))
      '()
      (cons
       (listPickRandomItem (dominant->recessiveList (car ruleListList) (car dominantList)) )
       (getRandomPropertiesOfParent (cdr ruleListList) (cdr dominantList))
      )
  )
)

(define (choose item1 item2)
  (if (equal? (random 2) 0)
      item1
      item2
      )
  )

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

(define (mendel motherDominant fatherDominant anzahl)
  (let*
      (
        [motherRecessive (getRandomPropertiesOfParent mendelRegeln motherDominant)]
        [fatherRecessive (getRandomPropertiesOfParent mendelRegeln fatherDominant)]
        [motherRandom (map choose motherDominant motherRecessive)]
        [fatherRandom (map choose fatherDominant fatherRecessive)]
        [child (map find-first-of mendelRegeln (map list motherRandom fatherRandom))]
      )
      (display fatherRecessive)
      (display fatherRandom)
      (display (map list fatherRecessive fatherRandom))
      (display (map find-first-of mendelRegeln (map list motherRandom fatherRandom)))
      (show-butterfly (car child) (cadr child) (caddr child) (cadddr child))
    )
  )


(mendel '( blue stripes curved hexagon ) '( yellow dots straight rhomb ) 1)