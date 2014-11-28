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


;KOMMENTAR: 28 / 30 Punkte


; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################

;KOMMENTAR: 6 Pkt

;(max (min 2 (- 2 5)) 0)
; -> 0
; Da:
;  min von 2 und -3 -> -3
;  min von -3 und 0 ->  0 KOMMENTAR: max nicht min ;)

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

;KOMMENTAR: 4 Pkt

#|

<Notmeldung>        ::= <Überschrift> <Standortangabe> <Art> <weiterAngaben> -- <Unterschrift> <Over>

<Überschrift>       ::= <Notzeichen> <Notzeichen> <Notzeichen>
                        Hier ist
                        <Schiffsname> <Schiffsname> <Schiffsname>
                        <RufzeichenB>
                        <Notzeichen> <SchiffsnameB> <RufzeichenB>
<Standortangabe>    ::= <Satz>
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
; ## Funktion aus dem letztem Blatt ############################################
; ##############################################################################

(define BuchstabierTafel 
  '(
    (#\A "Alfa")
    (#\T "Tango")
    (#\B "Bravo")
    (#\U "Uniform")
    (#\C "Charlie")
    (#\V "Viktor")
    (#\D "Delta")
    (#\W "Whiskey")
    (#\E "Echo")
    (#\X "X-ray")
    (#\F "Foxtrott")
    (#\Y "Yankee")
    (#\G "Golf")
    (#\Z "Zulu")
    (#\H "Hotel")
    (#\0 "Nadazero")
    (#\I "India")
    (#\1 "Unaone")
    (#\J "Juliett")
    (#\2 "Bissotwo")
    (#\K "Kilo")
    (#\3 "Terrathree")
    (#\L "Lima")
    (#\4 "Kartefour")
    (#\M "Mike")
    (#\5 "Pantafive")
    (#\N "November")
    (#\6 "Soxisix")
    (#\O "Oscar")
    (#\7 "Setteseven")
    (#\P "Papa")
    (#\8 "Oktoeight")
    (#\Q "Quebec")
    (#\9 "Novenine")
    (#\R "Romeo")
    (#\, "Decimal")
    (#\S "Sierra")
    (#\. "Stop")
    )
  )
; Gibt den Schlüssel zu einem Char aus 'BuchstabierTafel' zurück
(define (Buchstabe->tafelwort Buchstabe)
  ( cadr
    (assoc Buchstabe BuchstabierTafel)
  )
)
(define (charlist->tafelwort charlist) ; charList = Die List der Buchstaben (char) die umgewandelt werden sollen.
  (
   if (empty? charlist)
    ; Es gibt keine weiteren Buchstaben, also eine leere Liste zurückgeben
    '()
    ; Sonst:
    (
      ; Eine Liste erstellen aus:
      cons 
      ; Dem aktuellen Buchstaben, welcher in das tafelwort 'übersetzt' werden muss
      (Buchstabe->tafelwort 
        ; unter Berücksichtigung des Parameters, welcher der erste Eintrag in der Liste ist.
        (car charlist)
      )
      ; Zweites Element der neuen Liste ist der rekursive Aufruf mit dem Rest der Liste
      (charlist->tafelwort (cdr charlist))
    )
  )
)
; Wandelt einen String in eine Liste von Chars
(define (wort->tafelwort wort) ; wort = Ein String, der 'übersetzt' wird.
  (
    charlist->tafelwort (string->list wort)
  )
)


; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################

;KOMMENTAR: 7 Pkt

;; allgemeine Hilfsmethode
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

;; Gibt die Notmeldung aus
(define (notmeldung schiffsname rufzeichen position zeit art weitereAngaben)
  (string-append
    ( ueberschrift schiffsname rufzeichen )
    ( standortangabe position)
    ( zeitangabe zeit)
    ( artangabe art)
    ( extraangabe weitereAngaben)
    ( peilzeichen )
    ( unterschrift schiffsname rufzeichen )
    ( over )
  )
)

; Hilfsmethoden (Umsetzung der Backus-Naur-Form)

;; Hilfsmethode für die Überschrift
(define (ueberschrift schiffsname rufzeichen)
  ( let ( [notzeichen "MAYDAY"] )
     (string-append
       
       ( repeatString notzeichen 3 )
       
       "\nHIER IST \n"
       
       ( repeatString schiffsname 3 )
       " "
       ( string-join ( wort->tafelwort rufzeichen ) " " )
       "\n"
       
       notzeichen
       " "
       schiffsname
       " ICH BUCHSTABIERE "
       ( string-join ( wort->tafelwort schiffsname ) " " )
       
       "\nRUFZEICHEN "
       ( string-join ( wort->tafelwort rufzeichen ) " " )
       )
     )
  )
; Hilfsmethode für den Standort
(define (standortangabe standort) 
  (string-append
   "\nNOTFALLPOSITION " 
   standort
  )
)
; Hilfsmethode für die Zeit
(define (zeitangabe zeit) 
  (string-append
   "\nNOTFALLZEIT " 
   zeit
  )
)
; Hilfsmethode für die Art
(define (artangabe art) 
  (string-append
   "\n"
   art
  )
)
; Hilfsmethode für weitere Angaben (erwartet eine Liste)
(define (extraangabe weitereAngaben)
  (string-append
   "\n"
   (string-join weitereAngaben "\n")
  )
)
; Hilfsmethode fürs Peilzeichen
(define (peilzeichen) 
  " --\n"
)
; Hilfsmethode für die Unterschrift
(define (unterschrift schiffsname rufzeichen)       
  (string-append
   schiffsname
   " "
   ( string-join ( wort->tafelwort rufzeichen ) " " )
  )
)
; Hilfsmethode fürs Over
(define (over) 
  "\nOVER"
)




; ##############################################################################
; Aufgaben 2.3  ################################################################
; ##############################################################################

;KOMMENTAR: 3 Pkt

; Aufruf für Babette
(display ( notmeldung "BABETTE"
             "DEJY"
             "UNGEFAEHR 10 SM NORDOESTLICH LEUCHTTURM KIEL"
             "1000 UTC"
             "SCHWERER WASSEREINBRUCH WIR SINKEN"
             '("KEINE VERLETZTEN"
               "VIER MANN GEHEN IN DIE RETTUNGSINSEL"
               "SCHNELLE HILFE ERFORDERLICH" "ICH SENDE DEN TRAEGER")
             )
)

(display "\n\n")

; Aufruf für Amira
(display ( notmeldung "AMIRA"
             "AMRY"
             "UNGEFAEHR AUF DER HOEHE 53°56'N UND DER BREITE 006°31'E"
             "1640 UTC"
             "NACH KENTERGANG AM SINKEN"
             '("15 MANN AN BORD"
               "DAS SCHIFF IS 15M LANG"
               "ROTER RUMPF")
             )
)




; ##############################################################################
; ## Aufgabe 3 #################################################################
; ##############################################################################

;KOMMENTAR: 8 Pkt

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

--> (hoch3 (* 3 (+ 1 (hoch3 2)))) ;
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


;KOMMENTAR: Zu was für ein Problem führt das hier wirklich? Das new-if wird nicht terminieren!
Bei selbstgeschriebenen Funktionen arbeitet Racket nach der inneren Reduktion,
was bei dem new-if in der Funktion faculty zu einem unnötig höheren Rechen-
aufwand führen würde, da die else-clause berechnet werden würde, bevor geprüft wird,
ob dieser Fall eintritt.

|#