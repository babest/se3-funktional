#lang swindle

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

;KOMMENTAR: sehr schön! 40 / 40 Punkte

; ##############################################################################
; ## Aufgabe 1.1 ###############################################################
; ##############################################################################
;KOMMENTAR: 10 Pkt
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
     :initvalue 0
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
;KOMMENTAR: 5 Pkt
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

;(displayln ( cite abook ) )
;(displayln ( cite amiscellany ) )
;(displayln ( cite apaper ) )



; ##############################################################################
; ## Aufgabe 1.3 ###############################################################
; ##############################################################################

#|
;KOMMENTAR: 5 Pkt
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



; ##############################################################################
; ## Aufgabe 2.1 + 2.2 #########################################################
; ##############################################################################
;KOMMENTAR: sehr schön, 10 Pkt
; Klasse für das grundsätzliche Fahrzeug
( defclass fahrzeug ()
   ( medium ; Medium
     :reader get-medium
     :initarg :medium
     :initvalue ""
     :type <string>
     :documentation "The medium the vehicle is moving in"
     )
   ( maxspeed ; Maximalgeschwindigkeit
     :reader get-maxspeed
     :initarg :maxspeed
     :initvalue 0
     :type <number>
     :documentation "The maximal speed of the vehicle"
     )
   ( capacity ; Zuladung
     :reader get-capacity
     :initarg :capacity
     :initvalue 0
     :type <number>
     :documentation "The capacity of the vehicle"
     )
   ( consumption ; Verbrauch
     :reader get-consumption
     :initarg :consumption
     :initvalue 0
     :type <number>
     :documentation "The consumption of the vehicle"
     )
   ( maxPassengers ; Passagierzahl
     :reader get-maxPassengers
     :initarg :maxPassengers
     :initvalue 0
     :type <number>
     :documentation "The maximum about of the vehicle"
     )
   :printer #t
   )

; Erbende Klassen
( defclass landFahrzeug (fahrzeug)
   ( medium :initvalue "land")
   :printer #t
   )
( defclass landFahrzeugStrasse (landFahrzeug)
   ( medium :initvalue "landStrasse")
   :printer #t
   )
( defclass landFahrzeugSchienen (landFahrzeug)
   ( medium :initvalue "landSchiene")
   :printer #t
   )

( defclass wasserFahrzeug (fahrzeug)
   ( medium :initvalue "wasser")
   :printer #t
   )
( defclass luftFahrzeug (fahrzeug)
   ( medium :initvalue "luft")
   :printer #t
   )

( defclass amphibienFahrzeug (landFahrzeug wasserFahrzeug)
   :printer #t
   )
( defclass amphibienFlugzeug (luftFahrzeug landFahrzeugStrasse wasserFahrzeug)
   :printer #t
   )

( defclass zweiWegeFahrzeug (landFahrzeugStrasse landFahrzeugSchienen)
   :printer #t
   )
( defclass backIntoTheFutureCar (landFahrzeugSchienen luftFahrzeug)
   :printer #t
   )

; Generische Methoden
( defgeneric read-medium ((f fahrzeug)) :combination generic-append-combination )
( defgeneric read-maxspeed ((f fahrzeug)) :combination generic-min-combination )
( defgeneric read-capacity ((f fahrzeug)) :combination generic-min-combination )
( defgeneric read-consumption ((f fahrzeug)) :combination generic-max-combination )
( defgeneric read-maxPassengers ((f fahrzeug)) :combination generic-min-combination )

#|
Für die Methodekombination bieten sich verschiedene Möglichkeiten an.

Bei dem Medium müssen alle Medien ausgelistet sein; es darf keine überschrieben werden.

Für die meisten Eigenschaften muss entweder von einer Mindest- oder Maximalangabe
ausgegangen werden. Wir haben uns für eine Mindestangabe entschieden, bzw. Angaben
die die Fahrzeuge auf jeden Fall erfüllen, vielleicht auch mehr...
So ist ein Fahrzeug (hoffentlich) mindestens so schnell (maxspeed), wie das langsamste
der 'Abstammungsfahrzeuge', gleiches gilt für die Zuladung und auch die Anzahl der Passagiere.
Bei dem Verbrauch muss hingegen vom maximalen Verbrauch ausgegangen werden, da dieser bei
einseitiger Benutzung des Fahrzeug erreicht werden kann und somit die Reichweite einschränkt.
|#

; ##############################################################################
; ## Aufgabe 2.3 ###############################################################
; ##############################################################################
;KOMMENTAR: 10 Pkt
; Impementation EINER generische Methode
(defmethod read-maxspeed ((lf landFahrzeug))
  (get-maxspeed lf))
(defmethod read-maxspeed ((lfst landFahrzeugStrasse))
  (get-maxspeed lfst))
(defmethod read-maxspeed ((lfsc landFahrzeugSchienen))
  (get-maxspeed lfsc))
(defmethod read-maxspeed ((wf wasserFahrzeug))
  (get-maxspeed wf))
(defmethod read-maxspeed ((lf luftFahrzeug))
  (get-maxspeed lf))
(defmethod read-maxspeed ((af amphibienFahrzeug))
  (get-maxspeed af))
(defmethod read-maxspeed ((btf amphibienFlugzeug))
  (get-maxspeed btf))
(defmethod read-maxspeed ((zwf zweiWegeFahrzeug))
  (get-maxspeed zwf))
(defmethod read-maxspeed ((bitfc backIntoTheFutureCar))
  (get-maxspeed bitfc))


(define auto (make landFahrzeugStrasse :maxspeed 230 :capacity: 2000 :consumption 10 :maxPassengers 5))
(define ship (make wasserFahrzeug :maxspeed 50 :capacity: 200000 :consumption 100 :maxPassengers 500))
(define batMobil (make amphibienFlugzeug :maxspeed 300 :capacity: 1000 :consumption 0 :maxPassengers 2))
(define docsCar (make backIntoTheFutureCar :maxspeed 100000 :capacity: 300 :consumption 30 :maxPassengers 5))

; Beispielaufrufe
(displayln (read-maxspeed auto))
(displayln (read-maxspeed ship))
(displayln (read-maxspeed batMobil))
(displayln (read-maxspeed docsCar))

#|

In CLOS existiert zu jeder Klasse eine Klassenpräzedenzsliste. Dadurch wird die
Objektorientierung ermöglicht. Es ist wichtig, die Klassen von denen geerbit wird
in der richtige Reihenfolge zu definieren, ansonst gibt es einen Fehler, da CLOS
dann die Vererbungsstruktur nicht richtig aufbauen kann. Durch diese Vererbungs-
struktur wird dann auch die Reihenfolge der Abarbeitung der Methoden festgelegt.
Zuerst wird die Methode der Klasse mit der höchsten Präzendenz ausgeführt und dann
falls eine combination-Regel existiert, auch die anderen tieferliegenden Klassen
berücksichtigt.

|#