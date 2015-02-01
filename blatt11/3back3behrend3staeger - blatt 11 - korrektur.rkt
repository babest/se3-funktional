#lang lazy

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 11
  
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

#|

(gebaeude ?Haus weiss)
(gebaeude Informatikum ?Farbe)
-> ((?Farbe . weiss) (?Haus . Informatikum))

(Karten ((k Pik As) (k Herz Dame)))
(Karten ((k Pik As) (k Herz Koenig)))
-> Keine Unifikation, da keine Variablen
;KOMMENTAR: Keine Unifikation, da nicht unifizierbar. Nix Variablen.

(Karten ((k Pik As) (k Herz Dame)))
(Karten ((k Pik As) (k Herz ?Farbe)))
-> Keine Unifikation, da die Namen der Prädikate nicht stimmen
   (hier müsste nach Wert oder so gefragt werden)
;KOMMENTAR: Die Variable ?Farbe wird an Dame gebunden!
;KOMMENTAR: Damit ist es unifizierbar.

(Karten ((k Pik As) (k Herz Dame)))
(Karten ((k Pik As) (k Herz ?Farbe)))
-> Keine Unifikation, da die Namen der Prädikate nicht stimmen
   (hier müsste nach ?wert oder so gefragt werden)
;KOMMENTAR: Siehe Kommentar darüber

(Karten ((k Pik As) . ?andere))
(Karten ((k Pik As) (k Herz Koenig) (k Kreuz Dame))
-> Keine Unifikation, da Stelligkeit nicht stimmt
;KOMMENTAR: ?andere = '((k Herz Koenig) (k Kreuz Dame))

(Paar (k ?farbe As ) (k Pik ?wert))
(Paar (k Pik ?wert) (k ?farbe As))
-> ((?farbe . Pik) (?wert . As))

(Paar (k ?farbe As ) (k Pik ?wert2))
(Paar (k Pik ?wert) (k ?farbe ?wert))
-> ((?wert . As) (?wert2 . ?wert) (?farbe . Pik))
;KOMMENTAR: ?farbe = Pik, ?wert = As, ?wert2 = As
;KOMMENTAR: Man kann eine Variable nicht an eine Variable binden.
;KOMMENTAR: Eine Variable hat schließlich keinen Wert.
;KOMMENTAR: Was hier passiert nennt sich Koreferenzierung

|#



; ##############################################################################
; ## Aufgabe 1.2 ###############################################################
; ##############################################################################

;KOMMENTAR: Sieht gut aus

(require (lib "prologInScheme.ss" "se3-bib" "prolog"))

; ( ausleihe Signatur Lesernummer )
(<- ( ausleihe "K 110" 100 ) )
(<- ( ausleihe "P 30" 102 ) )
(<- ( ausleihe "P 32" 104 ) )
(<- ( ausleihe "P 50" 104 ) )
; ( vorbestellung Signatur Lesernummer )
(<- ( vorbestellung "K 110" 104 ) )
(<- ( vorbestellung "K 110" 102 ) )
(<- ( vorbestellung "P 30" 100 ) )
(<- ( vorbestellung "P 30" 104 ) )
; ( leser Name Vorname Lesernummer Geburtsjahr )
(<- ( leser Neugierig Nena 100 1989 ) )
(<- ( leser Linux Leo 102 1990 ) )
(<- ( leser Luator Eva 104 1988 ) )

; 1 Ist das Buch mit der Signatur K 110 ausgeliehen?
; (?- (ausleihe "K 110" ?leser))

; 2 Welche Lesernummer hat Leo Linux?
; (?- (leser Linux Leo ?lesernummer ?))

; 3 Welcher Leser (identifiziert durch Name und Vorname) hat das Buch mit der Signatur P 30 vorbestellt?
; (?- (vorbestellung "P 30" ?lesernummer)
;     (leser ?name ?vorname ?lesernummer ?))

; 4 Welche Leser (identifiziert durch Namen und Vornamen), die älter als 60 Jahre sind, haben ein Buch ausgeliehen?
; (?- (ausleihe ? ?lesernummer)
;     (leser ?name ?vorname ?lesernummer ?jahr)
;     (test (> (- 2015 60) ?jahr)))

; 5 Welche Leser haben mehr als ein Buch ausgeliehen?
; (?- (leser ?name ?vorname ?lesernummer ?)
;     (ausleihe ?buch1 ?lesernummer)
;     (ausleihe ?buch2 ?lesernummer)
;     (!= ?buch1 ?buch2)
;     (test (string<? ?buch1 ?buch2)))


; ##############################################################################
; ## Aufgabe 2 #################################################################
; ##############################################################################

;KOMMENTAR: Sieht gut aus

; Stream der natürlichen Zahlen (wichtig: Lazy Racket benutzen, sonst entsteht hier
; endlos Schleife (Rekursive Listendefinition)
(define (natsAbN n)
  (cons n (natsAbN (+ 1 n)))
  )

; Stream, der alle Siebenen und durch sieben teilbare Zahlen durch 'sum' ersetzt
(define (siebenStream stream)
  (let
      (
       ; Das aktuelle Element überprüpfen
       [currentElement 
        (cond
          ; Ist es durch Sieben teilbar, dann 'sum'
          [(= (remainder (car stream) 7) 0) "sum"]
          ; Enthält es eine Sieben, dann 'sum'
          [(regexp-match? #rx"7" (number->string (car stream)) ) "sum"]
          ; Sonst, das ganz normale aktuelle Element
          [else (car stream)]
          )
        ]
       )
    ; Stream rekursiv für alle folgenden Elemente definieren
    (cons currentElement (siebenStream (cdr stream)))
    )
  )
; Den Stream definieren
(define dieBoesenSieben (siebenStream (natsAbN 1)))
; (!! (take 40 dieBoesenSieben))



; ##############################################################################
; ## Aufgabe 3 #################################################################
; ##############################################################################

;KOMMENTAR: Alles gut

; Implementation
(define (fibu n)
  (cond 
    [(= n 0) 0]
    [(= n 1) 1]
    [else
     (+
      (fibu (- n 1))
      (fibu (- n 2))
      )]
    )
  )

; Implementation
(define (memori fn)
  (letrec
      ([table '()]
       [store (lambda (arg val)
                (set! table 
                      (cons (cons arg val) table)
                      ) val)]
       [retrieve
        (lambda (arg)
          (let ((val-pair (assoc arg table)))
            (if 
             val-pair 
             (cdr val-pair)
             #f
             )
            )
          )
        ]
       [ensure-val
        (lambda (x)
          (let ([stored-val (retrieve x)])
            (if
             stored-val
             stored-val
             (store x (fn x))
             )
            )
          )]
       )
    ensure-val
    )
  )

; Taken from the slides
(define memo-fib (memori fibu))
(set! fibu (memori fibu))

; Is working, but not very performant
(memo-fib 30)
