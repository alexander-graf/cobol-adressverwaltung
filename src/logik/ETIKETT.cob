       IDENTIFICATION DIVISION.
       PROGRAM-ID. ETIKETT.
      * Schreibt die Anschriften aller Treffer einer Suche als
      * Etiketten in eine Textdatei (UTF-8), getrennt durch eine
      * Leerzeile. Mit AU-MIT-ANREDE = "J" folgt die Briefanrede.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY zeichen.
       COPY zeile-d.
       01  W-I                     PIC 9(4).
       01  W-Z                     PIC 9.
       LINKAGE SECTION.
       COPY suche.
       COPY austausch.
       PROCEDURE DIVISION USING SU-PARM AU-PARM.
           MOVE 0 TO AU-ANZAHL
           MOVE "J" TO AU-OK
           MOVE SPACES TO AU-MELDUNG
           MOVE "N" TO W-MODUS
           CALL "ADRVPFAD" USING AU-DATEI W-AUS-PFAD
           PERFORM AUSGABE-OEFFNEN
           IF W-AUS-RC NOT = 0
               MOVE "N" TO AU-OK
               MOVE "Datei lässt sich nicht anlegen" TO AU-MELDUNG
               GOBACK
           END-IF
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > SU-ANZAHL
               MOVE ST-NR(W-I) TO KT-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK
                   PERFORM ETIKETT-SCHREIBEN
               END-IF
           END-PERFORM
           PERFORM AUSGABE-SCHLIESSEN
           MOVE AU-ANZAHL TO W-ZAHL
           STRING FUNCTION TRIM(W-ZAHL) " Anschrift(en) geschrieben"
                  DELIMITED BY SIZE INTO AU-MELDUNG
           GOBACK.

       ETIKETT-SCHREIBEN.
           MOVE "ANSCHR" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF AU-ANZAHL > 0
               PERFORM ZEILE-AUSGEBEN
           END-IF
           PERFORM VARYING W-Z FROM 1 BY 1 UNTIL W-Z > DI-ANZ-ZEILEN
               MOVE DI-ZEILE(W-Z) TO W-FELD
               PERFORM WERT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-PERFORM
           IF AU-MIT-ANREDE = "J"
               PERFORM ZEILE-AUSGEBEN
               MOVE DI-BRIEFANR TO W-FELD
               PERFORM WERT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-IF
           ADD 1 TO AU-ANZAHL.

       COPY zeile-p.
