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



; ##############################################################################
; ## Aufgabe 1.1 ###############################################################
; ##############################################################################

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

