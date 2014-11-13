#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  Aufgabenblatt 02
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#

;KOMMENTAR: 28 / 29 Punkte

; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################
;KOMMENTAR: 9 Pkt
(define miau 'Plueschi )
(define katze miau)
(define tiger 'miau)

(define (welcherNameGiltWo PersonA PersonB)
  ( let
    (
      (PersonA 'Sam)
      (PersonC PersonA )
    )
    PersonC
  )
)

(define xs1 '(0 2 3 miau katze ))
(define xs2 (list miau katze ))
(define xs3 (cons katze miau ))

;miau
; -> 'Plueschi
; Dieses Symbol wurde der Konstanten zugewiesen

;katze
; -> 'Plueschi
; Dieses Symbol wurde der Konstanten indirekt zugewiesen über die Konstante miau

;tiger
; -> 'miau
; Dieses Symbol wurde der Kosntanten zugewiesen

;(quote katze)
; -> 'katze
; Die Schreibeweise 'katze ist eine Kurzform für (quote katze)

;(eval tiger)
; -> 'Plueschi
; eval verwendet das übergebene Symbol als Namespace und ruft diesen auf.
; da das übergebene Symbol in diesem Fall 'miau ist, probiert eval die Methode/Konstante
; mit dem Namen miau aufzurufen. Hierhinter verbirgt sich dann 'Plueschi.

;(eval katze)
; -> Plueschi: undefined;
; Da sich hinter Katze das Symbol 'Plueschi befindet und Plueschi nicht als Methode
; oder Konstante definiert ist, ist hier ein Fehler.

;(eval 'tiger)
; -> 'miau
; Durch das Symbol 'tiger wird die Konstante tiger ausgegeben, hinter der sich das
; Symbol 'miau verbirgt

;(welcherNameGiltWo 'harry 'potter)
; -> 'harry
; Da die Variablen, die im let-block stehen, erst im Rumpf des let-blocks zur
; Verfügung stehen, ist der Wert von PersonA beim zuweisen auf PersonC noch der
; Wert, der der Funktion übergeben wurde.

;(cdddr xs1)
; -> '(miau katze)
; xs1 ist eine Liste mit den Symbolen 0, 2, 3, 'miau, 'katze.
; cdr gibt eine Liste zurück, bei der das erste Element der übergebenen Liste
; entfernt wurde. cdddr ist eine Kurzschreibweise für ( cdr ( cdr ( cdr xs1 ) ) ).
; Es werden also die ersten drei Elemente entfernt.

;(cdr xs2)
; -> '(Plueschi)
; xs2 ist eine Liste, die aus den Werten der Konstanten miau und katze besteht.
; Diese entsprechen in unserem Beispiel beide den Symbolen 'Plueschi.
; Durch cdr wird das erste Symbol entfernt.

;(cdr xs3)
; -> 'Plueschi
; xs3 ist ein Paar aus den Werten der Konstanten katze und miau.
; Diese entsprechen in unserem Beispiel beide den Symbolen 'Plueschi.
; Durch cdr wird nur das zweite Element des Paars zurückgegeben.

;(eval (sqrt 3))
; -> 1.7320508075688772
; Eine Zahl kann immer evaluiert werden und bleibt damit die Zahl.

;(eval '(welcherNameGiltWo 'tiger 'katze))
; -> 'tiger
; Wird an eine eval eine Liste übergeben, ist das erste Element der Name der
; Methode und die darauf folgenden Elemente die Parameter. Es wir in unserem
; Fall also aufgerufen:
; (welcherNameGilt 'tiger 'katze)

;(eval (welcherNameGiltWo 'katze 'tiger))
; -> 'Plueschi
; In diesem Fall wird das Ergebnis des Funktionsaufrufs
; (welcherNameGiltWo 'katze 'tiger) evaluiert. Im detaill passiert:
; (welcherNameGiltWo 'katze 'tiger) -> 'katze
; (eval 'katze) -> 'Plueschi



; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################
;KOMMENTAR: 2 Pkt
;; Berechnet die Fakultät einer Zahl n
(define (fakultaet n)
  ( if
    ( zero? n )  ; Ist n==0 ?
    1            ; Wenn ja, gib eins zurück
    ( *          ; Wenn nein, berechne n * (n-1)!
      n
      ( fakultaet
        ( sub1 n )
      )
    )
  )
)

; Beispielaufrufe
;(fakultaet  0)  ; -> 1
;(fakultaet  1)  ; -> 1
;(fakultaet  2)  ; -> 2
;(fakultaet  3)  ; -> 6
;(fakultaet 10)  ; -> 3628800



; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################
;KOMMENTAR: 3 Pkt
;; Berechnet die Potenz r^n
;; Dabei ist r eine Rationale Zahl und n eine Natürliche Zahl
(define (power r n)
  ( if
    ( zero? n )  ; Ist n==0 ?
    1            ; Wenn ja, dann gib eins zurück, denn r^0 = 1
    ( if         ; Wenn nein, ...
      ( odd? n )        ; Ist n eine ungerade Zahl ?
      ( *               ; Wenn ja, dann berechne r * r^(n-1)
        r
        ( power
          r
          ( sub1 n )
        )
      )
      ( sqr             ; Wenn nein, dann berechne (r^(n/2))^2
        ( power
          r
          ( / n 2 )
        )
      )
    )
  )
)

; Beispielaufrufe
;(power 0 0)  ; -> 1
;(power 1 0)  ; -> 1
;(power 12345 0)  ; -> 1
;(power 5 1)  ; -> 5
;(power 2 1)  ; -> 2
;(power 2 2)  ; -> 4
;(power 2 3)  ; -> 8
;(power 2 4)  ; -> 16
;(power 2 10)  ; -> 1024



; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################
;KOMMENTAR: 6 Pkt
;; Gibt die Eulerzahl bis auf 1000 Stellen genau zurück
(define (eulerzahl)
  ( /
    ( euler
      0
      ( /
        1
        ( power 10 1000 )
      )
    )
    2
  )
)

;; Hilfsmethode
;; Berechnet die 2-fache Eulerzahl e ab dem Summanden acc bis
;; zu einer Genauigkeit von summand <= precision
(define (euler acc precision)
  ( let*
    (
      [ acc! ( fakultaet acc ) ]  ; acc! ist die Fakultät von acc
      [ summand ( / ( add1 acc ) acc! ) ]  ; summand/glied der Reihe ist (acc+1)/(acc!)
    )
    ( if
      ( <  ; Ist der aktuelle Summand kleiner als 1/(10^1000)?
        summand
        precision
      )
      summand  ; Wenn ja, dann einfach nur den Summanden zurückgeben
      ( +  ; Wenn nein, dann addieren wir zu diesem Summanden alle, die kleiner sind als dieser
        summand
        ( euler
          ( add1 acc )
          precision
        )
      )
    )
  )
)

; Beispielaufruf
( * ( eulerzahl ) ( power 10 1001 ) )



; ##############################################################################
; ## Aufgabe 2.4 ###############################################################
; ##############################################################################
;KOMMENTAR: 4 Pkt
(define (pi)
 ( *
   (pi-partial 0 100)
   4
 )
)
(define (pi-partial step max-steps)
  ( let*
    (
      [ vorzeichen ( if (even? step) 1 -1 ) ]
      [ summand ( * ( / 1 (add1 ( * step 2 ) ) ) vorzeichen ) ]
    )
    ( if
      ( < step max-steps )
      ( +
        summand
        ( pi-partial ( add1 step ) max-steps )
      )
      summand
    )
  )
)

(* (pi) ( power 10 1000 ) )

; ##############################################################################
; ## Aufgabe 3 #################################################################
; ##############################################################################
;KOMMENTAR: 4 Pkt
; Pair und Liste müssen getauscht werden, weil jede Liste auch ein Pair ist.
(define (type-of input)
  (cond
    [(boolean? input)   "Boolean"]
    [(pair? input)      "Pair"]
    [(list? input)      "List"]
    [(symbol? input)    "Symbol"]
    [(number? input)    "Number"]
    [(char? input)      "Char"]
    [(string? input)    "String"]
    [(vector? input)    "Vector"]
    [(procedure? input) "Procedure"]
  )
)

;( type-of ( * 2 3 4 ) ) ; Number. Ist schließlich eine Multiplikation
;( type-of ( not 42) )   ; Boolean. not gibt nur boolsche Werte zurück
;KOMMENTAR: '( eins zwei drei) ist hier eben eine Liste und soll auch als Liste erkannt werden.
;( type-of '( eins zwei drei ) ) ; Pair. Eine Liste besteht immer aus Paaren. (Ist auch gleichzeitig eine Liste)
;( type-of ' ( ) )       ; List. Diese Liste enthält keine Elemente (leere Liste). Somit auch keine Paare. Deswegen ist dies 'nur' eine Liste
;( define ( id z ) z )   ; Kurz die Identität definieren
;( type-of ( id sin ) )  ; procedure. Sin (auch über die Identität) bleibt eine Funktion.
;( type-of ( string-ref "Harry Potter und der Stein der Weisen" 3)) ; char. Das 3. Element (r) aus dem String ist ein Zeichen.
;( type-of ( lambda ( x ) x ) ) ; procedure. Lambda ist eine Funktion. Somit eine Procedure. (Wird nicht definiert. Somit nicht global nutzbar)
;( type-of type-of )     ; procedure. type-of ist eine Funktion. Somit procedure
;( type-of ( type-of type-of ) ) ; String. Wie eben, nur jetzt wird die Rückgabe "procedure" geprüpft, welches ein String ist.
