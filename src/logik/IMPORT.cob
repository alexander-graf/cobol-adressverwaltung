       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPORT.
      * Liest eine CSV-Datei im Format von EXPORT (UTF-8, Semikolon,
      * erste Zeile Überschrift) und legt die Kontakte neu an. Die
      * Nummern werden neu vergeben; Ansprechpartner werden über die
      * alte Firmennummer der neu angelegten Firma zugeordnet.
      * Dubletten werden übersprungen. Jede Zeile kommt mit Ergebnis
      * ins Protokoll AU-PROTOKOLL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EIN-DATEI ASSIGN TO W-PFAD
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS W-EIN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  EIN-DATEI.
       01  EIN-SATZ                PIC X(8000).
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY zeichen.
       COPY pruef.
       COPY zeile-d.
       01  W-PFAD                  PIC X(250).
       01  W-EIN-STATUS            PIC XX.
       01  W-ZEILEN-NR             PIC 9(6).
       01  W-ZEILEN-NR-A           PIC Z(5)9.
       01  W-LEN                   PIC 9(5).
       01  W-I                     PIC 9(5).
       01  W-J                     PIC 99.
       01  W-K                     PIC 9(3).
       01  W-Z                     PIC X.
       01  W-IN-QUOTES             PIC X.
       01  W-F-ANZ                 PIC 99.
       01  W-F                     PIC X(300) OCCURS 40.
       01  W-F-POS                 PIC 9(3).
       01  W-TEIL                  PIC X(300).
       01  W-TEIL-POS              PIC 9(3).
       01  W-ZIEL                  PIC 9.
       01  W-ALT-NR                PIC X(7).
       01  W-ZIEL-NR               PIC X(7).
       01  W-MAP-ANZ               PIC 9(4).
       01  W-MAP                   OCCURS 3000.
           05  W-MAP-ALT           PIC X(7).
           05  W-MAP-NEU           PIC X(7).
       01  W-ERGEBNIS              PIC X(80).
       01  W-SICHER                PIC X(781).
       01  W-ZEILE-ERSETZT         PIC 9(5).
       LINKAGE SECTION.
       COPY austausch.
       PROCEDURE DIVISION USING AU-PARM.
           MOVE 0 TO AU-ANZAHL AU-UEBERSPRUNGEN AU-ERSETZT
                     W-ZEILEN-NR W-MAP-ANZ
           MOVE "J" TO AU-OK
           MOVE SPACES TO AU-MELDUNG
           MOVE "N" TO W-MODUS
           CALL "ADRVPFAD" USING AU-DATEI W-PFAD
           CALL "ADRVPFAD" USING AU-PROTOKOLL W-AUS-PFAD
           OPEN INPUT EIN-DATEI
           IF W-EIN-STATUS NOT = "00"
               MOVE "N" TO AU-OK
               STRING "Datei nicht lesbar, Status " W-EIN-STATUS
                      DELIMITED BY SIZE INTO AU-MELDUNG
               GOBACK
           END-IF
           PERFORM AUSGABE-OEFFNEN
           PERFORM UNTIL W-EIN-STATUS NOT = "00"
               READ EIN-DATEI
               IF W-EIN-STATUS = "00"
                   ADD 1 TO W-ZEILEN-NR
                   IF W-ZEILEN-NR > 1
                       PERFORM ZEILE-VERARBEITEN
                   END-IF
               END-IF
           END-PERFORM
           CLOSE EIN-DATEI
           PERFORM AUSGABE-SCHLIESSEN
           MOVE AU-ANZAHL TO W-ZAHL
           MOVE AU-UEBERSPRUNGEN TO W-ZAHL2
           MOVE AU-ERSETZT TO W-ZAHL3
           STRING FUNCTION TRIM(W-ZAHL) " übernommen, "
                  FUNCTION TRIM(W-ZAHL2) " übersprungen, "
                  FUNCTION TRIM(W-ZAHL3) " Zeichen ersetzt"
                  DELIMITED BY SIZE INTO AU-MELDUNG
           GOBACK.

       ZEILE-VERARBEITEN.
           IF EIN-SATZ = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION LENGTH(FUNCTION TRIM(EIN-SATZ TRAILING))
             TO W-LEN
      *    Windows-Zeilenende entfernen
           IF EIN-SATZ(W-LEN:1) = X"0D"
               SUBTRACT 1 FROM W-LEN
           END-IF
           MOVE "L" TO ZC-RICHTUNG
           MOVE W-LEN TO ZC-EIN-LEN
           MOVE EIN-SATZ TO ZC-EIN
           CALL "ZEICHEN" USING ZC-PARM
           MOVE ZC-ERSETZT TO W-ZEILE-ERSETZT
           ADD ZC-ERSETZT TO AU-ERSETZT
           PERFORM FELDER-ZERLEGEN
           PERFORM KONTAKT-AUFBAUEN
           IF W-ERGEBNIS = SPACES
               PERFORM KONTAKT-ANLEGEN
           ELSE
               ADD 1 TO AU-UEBERSPRUNGEN
           END-IF
           PERFORM PROTOKOLLIEREN.

      * CSV-Felder trennen: ";" außerhalb von Anführungszeichen,
      * "" innerhalb steht für ein ".
       FELDER-ZERLEGEN.
           MOVE 1 TO W-F-ANZ
           MOVE 0 TO W-F-POS
           MOVE SPACES TO W-F(1)
           MOVE "N" TO W-IN-QUOTES
           MOVE 1 TO W-I
           PERFORM UNTIL W-I > ZC-AUS-LEN
               MOVE ZC-AUS(W-I:1) TO W-Z
               EVALUATE TRUE
                   WHEN W-Z = QUOTE AND W-IN-QUOTES = "N"
                       MOVE "J" TO W-IN-QUOTES
                   WHEN W-Z = QUOTE AND ZC-AUS(W-I + 1:1) = QUOTE
                       PERFORM ZEICHEN-IN-FELD
                       ADD 1 TO W-I
                   WHEN W-Z = QUOTE
                       MOVE "N" TO W-IN-QUOTES
                   WHEN W-Z = ";" AND W-IN-QUOTES = "N"
                       IF W-F-ANZ < 40
                           ADD 1 TO W-F-ANZ
                           MOVE SPACES TO W-F(W-F-ANZ)
                           MOVE 0 TO W-F-POS
                       END-IF
                   WHEN OTHER
                       PERFORM ZEICHEN-IN-FELD
               END-EVALUATE
               ADD 1 TO W-I
           END-PERFORM
           PERFORM VARYING W-J FROM W-F-ANZ BY 1 UNTIL W-J >= 40
               MOVE SPACES TO W-F(W-J + 1)
           END-PERFORM.

       ZEICHEN-IN-FELD.
           IF W-F-POS < 300
               ADD 1 TO W-F-POS
               MOVE W-Z TO W-F(W-F-ANZ)(W-F-POS:1)
           END-IF.

       KONTAKT-AUFBAUEN.
           MOVE SPACES TO W-ERGEBNIS
           INITIALIZE KT-SATZ
           MOVE W-F(1) TO W-ALT-NR
           MOVE W-F(2) TO KT-ART
           IF NOT KT-IST-FIRMA AND NOT KT-IST-PERSON
               MOVE "übersprungen: Art muss F oder P sein"
                 TO W-ERGEBNIS
               EXIT PARAGRAPH
           END-IF
           MOVE W-F(3) TO KT-KATEGORIE
           MOVE W-F(4) TO W-TEIL
           MOVE 1 TO W-ZIEL
           PERFORM SCHLAGWORTE-TRENNEN
           IF KT-IST-FIRMA
               MOVE W-F(5) TO KF-NAME
               MOVE W-F(6) TO KF-ZUSATZ
               MOVE W-F(7) TO KF-RECHTSFORM
               MOVE W-F(8) TO KF-BRANCHE
               MOVE W-F(9) TO KF-WEBSITE
               MOVE W-F(10) TO KF-USTID
               MOVE W-F(11) TO KF-STEUERNR
               MOVE W-F(12) TO KF-REGGERICHT
               MOVE W-F(13) TO KF-HRNR
               MOVE W-F(14) TO KF-REFERENZ
               IF KF-NAME = SPACES
                   MOVE "übersprungen: Firmenname fehlt" TO W-ERGEBNIS
               END-IF
           ELSE
               MOVE W-F(15) TO KP-ANREDE
               MOVE W-F(16) TO KP-TITEL
               MOVE W-F(17) TO KP-VORNAME
               MOVE W-F(18) TO KP-VORNAMEN2
               MOVE W-F(19) TO KP-VORSATZ
               MOVE W-F(20) TO KP-NACHNAME
               MOVE W-F(21) TO KP-ZUSATZ
               MOVE "DATUM" TO PR-FUNKTION
               MOVE W-F(22) TO PR-WERT
               CALL "PRUEF" USING PR-PARM
               MOVE PR-DATUM TO KP-GEBDATUM
               MOVE W-F(24) TO KP-POSITION
               MOVE W-F(25) TO KP-ABTEILUNG
               PERFORM FIRMA-ZUORDNEN
               IF KP-NACHNAME = SPACES
                   MOVE "übersprungen: Nachname fehlt" TO W-ERGEBNIS
               END-IF
           END-IF
           MOVE W-F(38) TO W-TEIL
           PERFORM NOTIZ-TRENNEN
           IF W-F(39) = "J"
               MOVE "J" TO KT-ARCHIV
           ELSE
               MOVE "N" TO KT-ARCHIV
           END-IF.

      * Alte Firmennummer: zuerst in diesem Import angelegt, sonst
      * bereits im Bestand vorhanden, sonst bleibt sie ohne Firma.
       FIRMA-ZUORDNEN.
           IF W-F(23) = SPACES
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-MAP-ANZ
               IF W-MAP-ALT(W-I) = W-F(23)
                   MOVE W-MAP-NEU(W-I) TO KT-FIRMA-NR
               END-IF
           END-PERFORM
           IF KT-FIRMA-NR = SPACES
               MOVE KT-SATZ TO W-SICHER
               MOVE W-F(23) TO KT-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               MOVE W-SICHER TO KT-SATZ
               IF IO-OK
                   MOVE W-F(23) TO KT-FIRMA-NR
               END-IF
           END-IF.

       SCHLAGWORTE-TRENNEN.
           MOVE 0 TO W-TEIL-POS
           PERFORM VARYING W-K FROM 1 BY 1 UNTIL W-K > 300
               IF W-TEIL(W-K:1) = "," AND W-ZIEL < 5
                   ADD 1 TO W-ZIEL
                   MOVE 0 TO W-TEIL-POS
               ELSE
                   IF W-TEIL-POS < 11
                      AND NOT (W-TEIL-POS = 0 AND W-TEIL(W-K:1) = " ")
                       ADD 1 TO W-TEIL-POS
                       MOVE W-TEIL(W-K:1)
                         TO KT-SCHLAGWORT(W-ZIEL)(W-TEIL-POS:1)
                   END-IF
               END-IF
           END-PERFORM.

       NOTIZ-TRENNEN.
           MOVE 1 TO W-ZIEL
           MOVE 0 TO W-TEIL-POS
           PERFORM VARYING W-K FROM 1 BY 1 UNTIL W-K > 300
               IF W-TEIL(W-K:1) = "|" AND W-ZIEL < 4
                   ADD 1 TO W-ZIEL
                   MOVE 0 TO W-TEIL-POS
               ELSE
                   IF W-TEIL-POS < 60
                      AND NOT (W-TEIL-POS = 0 AND W-TEIL(W-K:1) = " ")
                       ADD 1 TO W-TEIL-POS
                       MOVE W-TEIL(W-K:1)
                         TO KT-NOTIZ(W-ZIEL)(W-TEIL-POS:1)
                   END-IF
               END-IF
           END-PERFORM
           CONTINUE.

       KONTAKT-ANLEGEN.
           MOVE "DUBLETTE" TO DI-FUNKTION
           MOVE W-F(30) TO DI-PLZ
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF DI-TREFFER-NR NOT = SPACES
               ADD 1 TO AU-UEBERSPRUNGEN
               STRING "übersprungen: Dublette zu " DI-TREFFER-NR
                      DELIMITED BY SIZE INTO W-ERGEBNIS
               IF KT-IST-FIRMA AND W-ALT-NR NOT = SPACES
                   MOVE DI-TREFFER-NR TO W-ZIEL-NR
                   PERFORM ZUORDNUNG-MERKEN
               END-IF
               EXIT PARAGRAPH
           END-IF
           MOVE "NEU" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF NOT DI-ERFOLG
               MOVE DI-MELDUNG TO W-ERGEBNIS
               ADD 1 TO AU-UEBERSPRUNGEN
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO AU-ANZAHL
           STRING "übernommen als " KT-NR DELIMITED BY SIZE
             INTO W-ERGEBNIS
           IF KT-IST-FIRMA AND W-ALT-NR NOT = SPACES
               MOVE KT-NR TO W-ZIEL-NR
               PERFORM ZUORDNUNG-MERKEN
           END-IF
           IF W-F(27) NOT = SPACES OR W-F(30) NOT = SPACES
              OR W-F(31) NOT = SPACES
               INITIALIZE AD-SATZ
               MOVE KT-NR TO AD-KT-NR
               MOVE W-F(26) TO AD-TYP
               IF AD-TYP = SPACE
                   MOVE "H" TO AD-TYP
               END-IF
               MOVE "J" TO AD-STANDARD
               MOVE W-F(27) TO AD-STRASSE
               MOVE W-F(28) TO AD-HAUSNR
               MOVE W-F(29) TO AD-ZUSATZ
               MOVE W-F(30) TO AD-PLZ
               MOVE W-F(31) TO AD-ORT
               MOVE W-F(32) TO AD-ORTSTEIL
               MOVE W-F(33) TO AD-REGION
               MOVE W-F(34) TO AD-LAND
               IF AD-LAND = SPACES
                   MOVE "DE" TO AD-LAND
               END-IF
               MOVE "ADR-NEU" TO DI-FUNKTION
               CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           END-IF
           MOVE "EM" TO KO-TYP
           MOVE W-F(35) TO KO-WERT
           PERFORM KOMM-ANLEGEN
           MOVE "TE" TO KO-TYP
           MOVE W-F(36) TO KO-WERT
           PERFORM KOMM-ANLEGEN
           MOVE "MO" TO KO-TYP
           MOVE W-F(37) TO KO-WERT
           PERFORM KOMM-ANLEGEN.

       KOMM-ANLEGEN.
           IF KO-WERT NOT = SPACES
               MOVE KT-NR TO KO-KT-NR
               MOVE SPACES TO KO-BEZ
               MOVE "J" TO KO-BEVORZUGT
               MOVE "KOM-NEU" TO DI-FUNKTION
               CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           END-IF.

       ZUORDNUNG-MERKEN.
           IF W-MAP-ANZ < 3000
               ADD 1 TO W-MAP-ANZ
               MOVE W-ALT-NR TO W-MAP-ALT(W-MAP-ANZ)
               MOVE W-ZIEL-NR TO W-MAP-NEU(W-MAP-ANZ)
           END-IF.

       PROTOKOLLIEREN.
           MOVE W-ZEILEN-NR TO W-ZEILEN-NR-A
           MOVE "Zeile " TO W-FELD
           MOVE 6 TO W-FLEN
           PERFORM ROH-ANHAENGEN
           MOVE W-ZEILEN-NR-A TO W-FELD
           MOVE FUNCTION TRIM(W-FELD) TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE ": " TO W-FELD
           MOVE 2 TO W-FLEN
           PERFORM ROH-ANHAENGEN
           MOVE W-ERGEBNIS TO W-FELD
           PERFORM TEXT-ANHAENGEN
           IF W-ZEILE-ERSETZT > 0
               MOVE " (unbekannte Zeichen durch ? ersetzt)" TO W-FELD
               PERFORM TEXT-ANHAENGEN
           END-IF
           PERFORM ZEILE-AUSGEBEN.

       COPY zeile-p.
