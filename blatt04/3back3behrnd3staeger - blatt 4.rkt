#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 04
  
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

;(max (min 2 (- 2 5)) 0)
; -> 0
; Da:
;  min von 2 und -3 -> -3
;  min von -3 und 0 ->  0

'(+ (- 2 13) 11)
; -> '(+ (- 2 13) 11)
; Da Das Quotezeichen das Nachfolgende als Symbol betrachtet, ohne es weiter auszuwerten

(cadr '(Alle Jahre wieder))
; -> 'Jahre
; Da cadr das zweite Element einer Liste zurückgibt.

(cddr '(kommt (das Weihnachtfest)))
; -> () (leere Liste)
; Da cddr das dritte (nicht vorhandene) Element zurück gibt.

(cdadr '(kommt (das . Weihnachtfest)))
; -> 'Weihnachtfest
; Da (cdr '(kommt (das . Weihnachtfest))) -> '((das . Weihnachtfest))
;  (Die Restliste ohne das erste Element)
; (car '((das . Weihnachtfest))) -> '(das . Weihnachtfest)
;  (Das erste Element (der jetzt einelementigen) Liste
; (cdr '(das . Weihnachtfest)) -> 'Weihnachtfest
;  Das zweite Element der/des Liste/Paar.

(cons 'Listen '(ganz einfach und))
; -> '(Listen ganz einfach und)
; cons erstellt ein Liste aus dem Symbol 'Listen und der Liste '(ganz einfach und)

(cons 'Paare 'auch)
; -> '(Paare . auch)
; cons erstellt in diesem Fall explizit ein Paar (Punktnotation), 
;  da als Parameter Symbole übergeben werden und nicht bereits Listen

(equal? (list 'Racket 'Prolog 'Java) '(Racket Prolog Java))
; #t (true)
; Die beiden Listen sind equal im Inhalt, unabhängig davon ob sie über list oder '() erstellt werden

(eq? (list 'Racket 'Prolog 'Java) (cons 'Racket '(Prolog Java)))
; #f
; Die beiden Listen sind allerdings nicht referenzgleich (eq). Es liegen zwei Listen im Speicher.



; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

#|

<Notmeldung>        ::= <Überschrift> <Standortangabe> <Art> <weiterAngaben> <Peilzeichen> <Unterschrift> <Over>

<Überschrift>       ::= <Notzeichen> <Notzeichen> <Notzeichen>
                        Hier ist
                        <Schiffsname> <Schiffsname> <Schiffsname>
                        <RufzeichenB>
                        <Notzeichen> <SchiffsnameB> <RufzeichenB>
<Standaortangabe>   ::= <Satz>
<Art>               ::= <Satz>
<weiterAngabe>      ::= <Satz>
<Peilzeichen>       ::= <Satz>
<Unterschrift>      ::= <Notzeichen> <Schiffsname> <SchiffsnameB> <RufzeichenB>
<Over>              ::= "OVER"

<Notzeichen>        ::= "MAYDAY"
<Schiffsname>       ::= <Wort>
<SchiffsnameB>      ::= <WortBuchstabiert>
<RufzeichenB>       ::= <WortBuchstabiert>

<Wort>              ::=  | <Buchstabe> <Wort>
<Satz>              ::=  | " "<Satz> | <Wort><Satz>
<Buchstabe>         ::= A | B | C | D | ... | Z | a | b | ... | z
<WortBuchstabiert>  ::=  | <CodeWort> <WortBuchtstabiert>
<CodeWort>          ::= Alfa | Bravo | Charlie | ... | Novenine | Decimal | Stop

|#



; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################

;; Gibt die Notmeldung aus
(define (notmeldung schiffsname rufzeichen position zeit art weitereAngaben)
  ( ueberschrift schiffsname rufzeichen )
  )


;; Hilfsmethode für die Überschrift
(define (ueberschrift schiffsname rufzeichen)
  ( let ( [notzeichen "MAYDAY"] )
     ( string-append
       ( repeatString notzeichen 3 )
       "HIER IST"
       ( repeatString schiffsname 3 )
       )
     )
  )


;; Hilfsmethode
;; Wiederholt einen String n-mal
(define (repeatString string n)
  ( if ( equal? n 1 )
       string
       ( string-append
         string
         ( if (> n 1) " " "" )  ; Ein Leerzeichen wird nicht nach dem letzten Wort benötigt.
         ( repeatString string (sub1 n))
         )
       )
  )




( notmeldung "BABETTE"
             "DEJY"
             "UNGEFAEHR 10 SM NORDOESTLICH LEUCHTTURM KIEL"
             "1000 UTC"
             "SCHWERER WASSEREINBRUCH WIR SINKEN"
             "KEINE VERLETZTEN ..." )




; ##############################################################################
; ## Aufgabe 3 #################################################################
; ##############################################################################

#|

Unterschied zwischen innerer und äußerer Reduktion:

Bei der inneren Reduktion werden die Terme von innen nach außen reduziert, was
dazu führt, dass zuerst die inneren Klammern bzw. Funktionen ausgewertet werden.

Bei der äußeren Reduktion werden die Terme von außen nach innen reduziert, also
werden zuerst die äußeren Klammern bzw. Funktionen ausgewertet, worauf die
inneren folgen.


Für (hoch3 (* 3 (+ 1 (hoch3 2))))

mit 
(define (hoch3 x) (* x x x))

folgt dann:

innere Reduktion:

--> (hoch3 (* 3 (+ 1 (* 2 2 2)))) ;(hoch3)
--> (hoch3 (* 3 (+ 1 8)))         ;(*)
--> (hoch3 (* 3 9))               ;(+)
--> (hoch3 27)                    ;(*)
--> (* 27 27 27)                  ;(hoch3)
--> 19683                         ;(*)


äußere Reduktion:

--> (* (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(hoch3)
--> (* (* 3 (+ 1 (* 2 2 2)))
       (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(hoch3)
--> (* (* 3 (+ 1 8))
       (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(*)
--> (* (* 3 9)
       (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(+)
--> (* 27
       (* 3 (+ 1 (hoch3 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(*)
--> (* 27
       (* 3 (+ 1 (* 2 2 2)))
       (* 3 (+ 1 (hoch3 2))))     ;(hoch3)
--> (* 27
       (* 3 (+ 1 8))
       (* 3 (+ 1 (hoch3 2))))     ;(*)
--> (* 27
       (* 3 9)
       (* 3 (+ 1 (hoch3 2))))     ;(+)
--> (* 27
       27
       (* 3 (+ 1 (hoch3 2))))     ;(*)
--> (* 27
       27
       (* 3 (+ 1 (* 2 2 2))))     ;(hoch3)
--> (* 27
       27
       (* 3 (+ 1 8)))             ;(*)
--> (* 27
       27
       (* 3 9))                   ;(+)
--> (* 27
       27
       27)                        ;(*)
--> 19683                         ;(*)


In Racket wird für Funktionen die innere Reduktion und für
Spezialformen (wie z.B. für if-Abfragen) die äußere Reduktion angewendet.


Bei selbstgeschriebenen Funktionen arbeitet Racket nach der inneren Reduktion,
was bei dem new-if in der Funktion faculty zu einem unnötig höheren Rechen-
aufwand führen würde, da die else-clause berechnet werden würde, bevor geprüft wird,
ob dieser Fall eintritt.

|#