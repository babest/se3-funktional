;#lang swindle

#|
  SE3 Funktionale Programmierung
  WiSe 2014, Uni Hamburg
  
  Gruppe 11, Do. 18:15
  Aufgabenblatt 09
  
  Eingereicht von
    Timon Back (3back)
    Fabian Behrendt (3behrend)
    Nicolai Stäger (3staeger)

  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#


( require swindle/setf swindle/misc )



; ##############################################################################
; ## Aufgabe 1.1 ###############################################################
; ##############################################################################

; Klasse für eine wissenschaftliche Veröffentlichung
( defclass scientific-publication ()
   ( key ; eindeutiger Schlüssel
     :reader get-key
     :initarg :key
     :initvalue 0
     :type <number>
     :documentation "Unique Key"
     )
   ( author ; Name der Autorinnen und Autoren
     :reader get-author
     :initarg :author
     :initvalue ""
     :type <string>
     :documentation "Name of the author"
     )
   ( year ; Erscheinungsjahr
     :reader get-year
     :initarg :year
     :initvalie 0
     :type <number>
     :documentation "The year of publication"
     )
   ( title ; Titel der Veröffentlichung
     :reader get-title
     :initarg :title
     :initvalue ""
     :type <string>
     :documentation "The title of the publication"
     )
   :printer #t
   )



; Klasse für ein Buch
( defclass book (scientific-publication)
   ( publisher ; Verlag
     :reader get-publisher
     :initarg :publisher
     :initvalue ""
     :type <string>
     :documentation "The name of the publisher"
     )
   ( publisher-place ; Verlagsort
     :reader get-publisher-place
     :initarg :publisher-place
     :initvalue ""
     :type <string>
     :documentation "The place of the publisher"
     )
   ( series ; Reihe
     :reader get-series
     :initarg :series
     :initvalue ""
     :type <string>
     :documentation "The series"
     )
   ( series-number ; Seriennummer der Reihe
     :reader get-series-number
     :initarg :series-number
     :initvalue 0
     :type <number>
     :documentation "The number of the series"
     )
   :printer #t
   )

; Klasse für einen Sammelband
( defclass miscellany (book)
   ( editor ; Herausgeber
     :reader get-editor
     :initarg :editor
     :initvalue ""
     :type <string>
     :documentation "Name of the editor"
     )
   ( page ; Seitenangabe
     :reader get-page
     :initarg :page
     :initvalue 0
     :type <number>
     :documentation "The page of the article"
     )
   :printer #t
   )

; Klasse für einen Zeitschriftenartikel
( defclass paper (scientific-publication)
   ( name ; Name der Zeitschrift
     :reader get-name
     :initarg :name
     :initvalue ""
     :type <string>
     :documentation "The name of the paper"
     )
   ( volume ; Nummer des Bandes
     :reader get-volume
     :initarg :volume
     :initvalue 0 
     :type <number>
     :documentation "The volume"
     )
   ( number ; Nummer des Heftes
     :reader get-number
     :initarg :number
     :initvalue 0
     :type <number>
     :documentation "The number of the magazin"
     )
   ( month ; Erscheinungsmonat
     :reader get-month
     :initarg :month
     :initvalue ""
     :type <string>
     :documentation "The month of publication"
     )
   :printer #t
   )

; Beispiel Daten
( define abook
   ( make book
          :key 1
          :author "Nessie"
          :year 1790
          :title "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
          :publisher "Minority-Verlag"
          :publisher-place "Inverness"
          :series "Die besondere Biographie"
          :series-number 2
          )
   )
( define amiscellany
   ( make miscellany
          :key 2
          :author "Perfect, F."
          :year 1979
          :title "Mostly harmless - some observations concerning the third planet of the solar sytem"
          :publisher "Galactic Press"
          :publisher-place "Vega-System, 3rd planet"
          :series "Travel in Style"
          :series-number 5
          :editor "Adams, D."
          :page 420
          )
   )
( define apaper
   ( make paper
          :key 3
          :author "Welss, H.G."
          :year 3200
          :title "Zeitmaschinen leicht gemacht"
          :name "Heimwerkerpraxis für Anfänger"
          :volume 550
          :number 3
          )
   )



; ##############################################################################
; ## Aufgabe 1.2 ###############################################################
; ##############################################################################

; Für die Ausgabe der Literaturbeiträge verwenden wir Methodenkombination
; Es wird also jede anwendbare Methode bei einem Aufruf von cite aufgerufen.
; Die Ergebnisse der einzelnen Methoden kombinieren wir dann zu einem Endergebnis.

; Unsere cite Methoden geben alle einen String zurück.
; Da Swindle keine Methode von Haus aus definiert, mit der man die Ergebnisse als
; Strings appenden kann, definieren wir unsere eigene Funktion zur Kombination.
; Der hier verwendete Parameter r enthält die Ergebnisse aller Methoden als Liste?,
; wobei das erste Element das Ergebniss der "obersten" Methode ist.
; Mehr Infos: https://github.com/racket/swindle/blob/master/tiny-clos.rkt#L1302
(define* string-prepend-combination
  ( make-generic-combination
    :process-result
    ( lambda (r)
       ( apply string-append r )
       )
    )
  )

; Beim definieren der generischen Funktion geben wir an, dass wir zum Kombinieren
; der Ergebnisse unsere eben definierte Methode verwenden wollen.
( defgeneric cite (( p scientific-publication )) :combination string-prepend-combination )

; Cite für eine Wissenschaftliche Veröffentlichung
( defmethod cite (( p scientific-publication ))
   ( string-append
     ( get-author p )
     " ("
     ( number->string ( get-year p ) )
     "). "
     ( get-title p )
     )
   
   )

; Cite für ein Buch
( defmethod cite (( b book ))
   ( string-append
     ", Band "
     ( number->string ( get-series-number b ) )
     " der Reihe: "
     ( get-series b )
     ", "
     ( get-publisher b )
     ", "
     ( get-publisher-place b )
     "."
     )
   )

; Cite für einen Sammelband
( defmethod cite (( m miscellany ))
   ( string-append
     ", S. "
     ( number->string ( get-page m ) )
     ", "
     (get-editor m )
     "."
     )
   )

; Cite für einen Zeitschriftenartikel
( defmethod cite (( p paper ))
   ( string-append
     ", "
     ( get-name p )
     ", "
     ( number->string ( get-volume p) )
     "("
     ( number->string ( get-number p ) )
     ")"
     )
   )

; Beispielaufrufe

;( cite abook )
;( cite amiscellany )
;( cite apaper )



; ##############################################################################
; ## Aufgabe 1.3 ###############################################################
; ##############################################################################

#|

Eine Ergänzungsmehtode ist eine Methode in einer Unterklasse, die das Verhalten
einer Methode in der Oberklasse ergänzt bzw. erweitert. Die Methoden haben dabei
beide den gleichen Namen, jedoch wird in der Unterklasse die Methode mit den
Schlüsselworten :before :after oder :around markiert um anzugeben, dass die 
gleichnamige Methode in der Unterklasse, NICHT die aus der Oberklasse ersetzen
soll, sondern VOR oder NACH ihr zusätzlich aufgerufen werden soll.
Im Gegensatz zum super call ist bei Ergänzungsmethoden sichergestellt, daß alle
Ergänzungsmethoden ausgeführt werden. So können keine Initialisierungen vergessen
oder unterdrückt werden, die in den Oberklassen definiert wurden.

Im Kontext unserer Aufgabe kann das System der Ergänzungsmehtode bei der cite-
Methode verwendet werden. Hier kann durch eine Methode der Oberklasse eine
Standard-Ausgabe generiert werden mit den gemeinsamen Feldern und diese dann
von den Unterklassen vorher oder nachher ergänzt werden. Eine Umstrukturierung
könnte wie folgt aussehen:

Die cite-Methoden der Unterklassen mit dem :after Schlüsselwort erweitern.
In allen cite-Methoden keinen String zurückgeben, sondern direkt auf der
Konsole ausgeben.

|#


