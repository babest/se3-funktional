#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  Aufgabenblatt 03
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

(define BuchstabierTafel 
  '(
    (#\A Alfa)
    (#\T Tango)
    (#\B Bravo)
    (#\U Uniform)
    (#\C Charlie)
    (#\V Viktor)
    (#\D Delta)
    (#\W Whiskey)
    (#\E Echo)
    (#\X X-ray)
    (#\F Foxtrott)
    (#\Y Yankee)
    (#\G Golf)
    (#\Z Zulu)
    (#\H Hotel)
    (0 Nadazero)
    (#\I India)
    (1 Unaone)
    (#\J Juliett)
    (2 Bissotwo)
    (#\K Kilo)
    (3 Terrathree)
    (#\L Lima)
    (4 Kartefour)
    (#\M Mike)
    (5 Pantafive)
    (#\N November)
    (6 Soxisix)
    (#\O Oscar)
    (7 Setteseven)
    (#\P Papa)
    (8 Oktoeight)
    (#\Q Quebec)
    (9 Novenine)
    (#\R Romeo)
    ("," Decimal)
    (#\S Sierra)
    ("." Stop)
    )
  )
; Diese Assoziativliste speichert zu jedem Buchstaben einen Schlüssel.
; Die Liste so anzulegen, hat den Vorteil, dass sie assoziativ ist (es kann mit assoc darauf zugegriffen werden.
; Dadurch kann mit einem Schlüssel auf den Wert zugeriffen werden (map).


; ##############################################################################
; ## Aufgabe 1.2 ###############################################################
; ##############################################################################

; Gibt den Schlüssel zu einem Char aus 'BuchstabierTafel' zurück
(define (Buchstabe->tafelwort Buchstabe)
  (
   car
   (
    cdr (assoc Buchstabe BuchstabierTafel)
        )
   )
  )

;(Buchstabe->tafelwort #\B) ; -> '(Bravo)
;(Buchstabe->tafelwort 1)   ; -> '(Unaone)
;(Buchstabe->tafelwort ".") ; -> '(Stop)



; ##############################################################################
; ## Aufgabe 1.3 ###############################################################
; ##############################################################################
(define (buchstabe->tafelwort buchstabe)
  (
   Buchstabe->tafelwort ; Benutzt die vorhandene Funktion, allerdings eventuell mit anderen Parameter
   (
    if (char? buchstabe)
       (
        ; Buchstabe ist ein Char (vielleicht ein kleiner Buchstabe?)
        if (char-lower-case? buchstabe) 
           ; b ist klein geschrieben.
           ; Dann b zu einem Großbuchstaben machen.
           ; Kann dann an die Buchstabe->tafelwort übergeben werden
           (char-upcase buchstabe)
           ; b ist bereits groß geschrieben. Parameter kann einfach weiter gereicht werden
           buchstabe
           )
       ; Buchstabe ist kein Char (also eine Zahl oder . ,). Parameter kann einfach weiter gereicht werden.
       buchstabe
       )
   )
  )

;(buchstabe->tafelwort #\B) ; -> '(Bravo)
;(buchstabe->tafelwort #\b) ; -> '(Bravo)
;(buchstabe->tafelwort 1)   ; -> '(Unaone)
;(buchstabe->tafelwort ".") ; -> '(Stop)



; ##############################################################################
; ## Aufgabe 1.4 ###############################################################
; ##############################################################################

; Wandelt eine Liste von Chars in die dazugehörigen Buchstabentafel-Liste (rekursive Funktion)
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
       (buchstabe->tafelwort 
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

;(wort->tafelwort "Racket") ; '((Romeo) (Alfa) (Charlie) (Kilo) (Echo) (Tango))







(require se3-bib/flaggen-module)
;(require flaggen-module)
; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

(define FlaggenTafel 
  '(
    (#\A A)
    (#\B B)
    (#\C C)
    (#\D D)
    (#\E E)
    (#\F F)
    (#\G G)
    (#\H H)
    (#\I I)
    (#\J J)
    (#\K K)
    (#\L L)
    (#\M M)
    (#\N N)
    (#\O O)
    (#\P P)
    (#\Q Q)
    (#\R R)
    (#\S S)
    (#\T T)
    (#\U U)
    (#\V V)
    (#\W W)
    (#\X X)
    (#\Y Y)
    (#\Z Z)
    (0 Z0)
    (1 Z1)
    (2 Z2)
    (3 Z3)
    (4 Z4)
    (5 Z5)
    (6 Z6)
    (7 Z7)
    (8 Z8)
    (9 Z9)
    )
  )
; Diese Assoziativliste speichert zu jedem Buchstaben einen Schlüssel.
; Die Liste so anzulegen, hat den Vorteil, dass sie assoziativ ist (es kann mit assoc darauf zugegriffen werden.
; Dadurch kann mit einem Schlüssel auf den Wert zugeriffen werden (map).


; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################

; Gibt den Schlüssel zu einem Char aus FlaggenTafel zurück
(define (Buchstabe->flagge Buchstabe)
  (car
   (cdr (assoc Buchstabe FlaggenTafel))
   )
  )

; (Buchstabe->flagge #\B)

; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################

; Wandelt eine Liste von Chars in die dazugehörigen Buchstabentafel-Liste (rekursive Funktion)
(define (charlist->flaggenwort charlist) ; charList = Die List der Buchstaben (char) die umgewandelt werden sollen.
  (
   if (empty? charlist)
      ; Es gibt keine weiteren Buchstaben, also eine leere Liste zurückgeben
      '()
      ; Sonst:
      (
       ; Eine Liste erstellen aus:
       cons 
       ; Dem aktuellen Buchstaben, welcher in das tafelwort 'übersetzt' werden muss
       (Buchstabe->flagge 
        ; unter Berücksichtigung des Parameters, welcher der erste Eintrag in der Liste ist.
        (car charlist)
        )
       ; Zweites Element der neuen Liste ist der rekursive Aufruf mit dem Rest der Liste
       (charlist->flaggenwort (cdr charlist))
       )
      )
  )

; Wandelt einen String in eine Liste von Chars
(define (wort->flaggenwort wort) ; wort = Ein String, der 'übersetzt' wird.
  (
   charlist->flaggenwort (string->list wort)
                         )
  )

;(wort->flaggenwort "RACKET") ; '((Romeo) (Alfa) (Charlie) (Kilo) (Echo) (Tango))
