       IDENTIFICATION DIVISION.
       PROGRAM-ID. BGEB.
      * Geburtstage der nächsten Tage; eine Zeile öffnet die Person.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY geb.
       01  W-TAGE                  PIC 999 VALUE 30.
       01  W-EINGABE               PIC XX.
       01  W-I                     PIC 999.
       01  W-NR                    PIC 99.
       01  W-ZEILE                 PIC 99.
       01  W-ZL                    PIC Z9.
       01  W-TG                    PIC ZZ9.
       01  W-WIRD                  PIC ZZ9.
       01  W-ANZEIGE               PIC X(78).
       01  W-DATUM                 PIC 9(8).
       01  W-DATUM-X REDEFINES W-DATUM PIC X(8).
       01  W-FERTIG                PIC X.
       01  W-PERSON-NR             PIC X(7).
       01  W-RUECK                 PIC X(8).
       01  W-KEINE-FIRMA           PIC X(7) VALUE SPACES.
       SCREEN SECTION.
       01  S-RAHMEN.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE "Zeitraum (Tage):".
           05  LINE 3 COLUMN 19 PIC 999 USING W-TAGE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 5 COLUMN 2 VALUE
               "Zl  in Tagen  Datum   Name" HIGHLIGHT.
           05  LINE 5 COLUMN 70 VALUE "wird" HIGHLIGHT.
       01  S-AUSWAHL.
           05  LINE 3 COLUMN 19 PIC 999 USING W-TAGE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 21 COLUMN 2 VALUE "Zeile öffnen:".
           05  LINE 21 COLUMN 16 PIC XX USING W-EINGABE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
       PROCEDURE DIVISION.
           MOVE "Geburtstage" TO W-TITEL
           MOVE SPACES TO W-KOPF-RECHTS W-MELDUNG
           MOVE "Enter: Zeile öffnen oder Zeitraum übernehmen"
             TO W-TASTEN1
           MOVE "F3 zurück  F1 Hilfe" TO W-TASTEN2
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               MOVE FUNCTION CURRENT-DATE(1:8) TO GL-HEUTE
               MOVE W-TAGE TO GL-TAGE
               CALL "GEBLISTE" USING GL-PARM
               PERFORM LISTE-ZEIGEN
               MOVE SPACES TO W-EINGABE
               ACCEPT S-AUSWAHL
               MOVE SPACES TO W-MELDUNG
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "J" TO W-FERTIG
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-ENTER
                       PERFORM AUSWAHL
               END-EVALUATE
           END-PERFORM
           GOBACK.

       LISTE-ZEIGEN.
           IF GL-ANZAHL = 0
               MOVE "Keine Geburtstage in diesem Zeitraum."
                 TO W-MELDUNG
           END-IF
           DISPLAY S-RAHMEN
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > GL-ANZAHL OR W-I > 15
               MOVE W-I TO W-ZL
               MOVE GL-IN-TAGEN(W-I) TO W-TG
               MOVE GL-WIRD(W-I) TO W-WIRD
               MOVE GL-GEBDATUM(W-I) TO W-DATUM
               MOVE SPACES TO W-ANZEIGE
               MOVE W-ZL TO W-ANZEIGE(1:2)
               MOVE W-TG TO W-ANZEIGE(7:3)
               IF GL-IN-TAGEN(W-I) = 0
                   MOVE "heute" TO W-ANZEIGE(5:5)
               END-IF
               STRING W-DATUM-X(7:2) "." W-DATUM-X(5:2) "."
                      DELIMITED BY SIZE INTO W-ANZEIGE(14:6)
               MOVE GL-NAME(W-I) TO W-ANZEIGE(22:45)
               MOVE W-WIRD TO W-ANZEIGE(69:3)
               COMPUTE W-ZEILE = W-I + 5
               DISPLAY W-ANZEIGE LINE W-ZEILE COLUMN 2
           END-PERFORM.

       AUSWAHL.
           IF W-EINGABE = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO W-NR
           IF FUNCTION TEST-NUMVAL(W-EINGABE) = 0
               COMPUTE W-NR = FUNCTION NUMVAL(W-EINGABE)
           END-IF
           IF W-NR >= 1 AND W-NR <= GL-ANZAHL AND W-NR <= 15
               MOVE GL-NR(W-NR) TO W-PERSON-NR
               CALL "BPERSON" USING W-PERSON-NR W-RUECK W-KEINE-FIRMA
               PERFORM KOPF-BAUEN
           ELSE
               MOVE "Bitte eine Zeile der Liste." TO W-MELDUNG
           END-IF.

       COPY bildkopf-p.
