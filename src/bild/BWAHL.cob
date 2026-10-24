       IDENTIFICATION DIVISION.
       PROGRAM-ID. BWAHL.
      * Trefferliste: führt die Suche aus SU-PARM aus, zeigt je Seite
      * 15 Treffer und liefert die Auswahl (Zeile oder Nummer).
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY ioparm.
       COPY kontakt.
       01  W-EINGABE               PIC X(7).
       01  W-SEITE                 PIC 999.
       01  W-SEITEN                PIC 999.
       01  W-ERSTER                PIC 9(4).
       01  W-I                     PIC 9(4).
       01  W-NR                    PIC 9(4).
       01  W-ZEILE                 PIC 99.
       01  W-ZL                    PIC Z9.
       01  W-ANZEIGE               PIC X(78).
       01  W-INFO                  PIC X(40).
       01  W-ANZ-A                 PIC ZZZ9.
       01  W-SEITE-A               PIC ZZ9.
       01  W-SEITEN-A              PIC ZZ9.
       01  W-FERTIG                PIC X.
       LINKAGE SECTION.
       COPY suche.
       COPY bwahl.
       SCREEN SECTION.
       01  S-RAHMEN.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE
               "Zl  Nummer   Name" HIGHLIGHT.
           05  LINE 3 COLUMN 57 VALUE "PLZ    Ort" HIGHLIGHT.
       01  S-EINGABE.
           05  LINE 20 COLUMN 2 VALUE "Auswahl (Zeile oder Nummer):".
           05  LINE 20 COLUMN 31 PIC X(7) USING W-EINGABE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 20 COLUMN 42 PIC X(38) FROM W-INFO.
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
       PROCEDURE DIVISION USING SU-PARM BW-PARM.
           MOVE SPACES TO BW-RUECK BW-NR W-KOPF-RECHTS
           MOVE BW-MELDUNG TO W-MELDUNG
           MOVE SPACES TO BW-MELDUNG
           MOVE BW-TITEL TO W-TITEL
           MOVE "Enter wählen  F3 zurück  Bild auf/ab blättern"
             TO W-TASTEN1
           IF BW-MODUS = "S"
               MOVE "F5 neu  F11 Etiketten aller Treffer  F1 Hilfe"
                 TO W-TASTEN2
           ELSE
               MOVE "F1 Hilfe" TO W-TASTEN2
           END-IF
           PERFORM KOPF-BAUEN
           CALL "SUCHE" USING SU-PARM
           IF SU-ANZAHL = 0
               MOVE "Keine Treffer." TO W-MELDUNG
           END-IF
           IF SU-VOLL = "J"
               MOVE "Mehr als 500 Treffer, bitte Suche eingrenzen."
                 TO W-MELDUNG
           END-IF
           COMPUTE W-SEITEN = (SU-ANZAHL + 14) / 15
           IF W-SEITEN = 0
               MOVE 1 TO W-SEITEN
           END-IF
           MOVE 1 TO W-SEITE
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               PERFORM SEITE-ZEIGEN
               MOVE SPACES TO W-EINGABE
               ACCEPT S-EINGABE
               MOVE SPACES TO W-MELDUNG
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "ZURUECK" TO BW-RUECK
                       MOVE "J" TO W-FERTIG
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-F7 OR T-SEITE-ZURUECK
                       IF W-SEITE > 1
                           SUBTRACT 1 FROM W-SEITE
                       END-IF
                   WHEN T-F8 OR T-SEITE-VOR
                       IF W-SEITE < W-SEITEN
                           ADD 1 TO W-SEITE
                       END-IF
                   WHEN T-F5 AND BW-MODUS = "S"
                       MOVE "NEU" TO BW-RUECK
                       MOVE "J" TO W-FERTIG
                   WHEN T-F11 AND BW-MODUS = "S"
                       MOVE "ETIK" TO BW-RUECK
                       MOVE "J" TO W-FERTIG
                   WHEN T-ENTER
                       PERFORM AUSWAHL-PRUEFEN
               END-EVALUATE
           END-PERFORM
           GOBACK.

       SEITE-ZEIGEN.
           DISPLAY S-RAHMEN
           COMPUTE W-ERSTER = (W-SEITE - 1) * 15 + 1
           PERFORM VARYING W-I FROM W-ERSTER BY 1
                   UNTIL W-I > SU-ANZAHL OR W-I >= W-ERSTER + 15
               COMPUTE W-ZEILE = W-I - W-ERSTER + 4
               COMPUTE W-ZL = W-I - W-ERSTER + 1
               MOVE SPACES TO W-ANZEIGE
               MOVE W-ZL TO W-ANZEIGE(1:2)
               MOVE ST-NR(W-I) TO W-ANZEIGE(5:7)
               MOVE ST-NAME(W-I) TO W-ANZEIGE(14:40)
               MOVE ST-PLZ(W-I) TO W-ANZEIGE(56:6)
               MOVE ST-ORT(W-I) TO W-ANZEIGE(63:16)
               DISPLAY W-ANZEIGE LINE W-ZEILE COLUMN 2
           END-PERFORM
           MOVE SU-ANZAHL TO W-ANZ-A
           MOVE W-SEITE TO W-SEITE-A
           MOVE W-SEITEN TO W-SEITEN-A
           MOVE SPACES TO W-INFO
           STRING FUNCTION TRIM(W-ANZ-A) " Treffer, Seite "
                  FUNCTION TRIM(W-SEITE-A) "/"
                  FUNCTION TRIM(W-SEITEN-A)
                  DELIMITED BY SIZE INTO W-INFO.

      * "3" = dritte Zeile der Seite, "P000012" = Nummer direkt
       AUSWAHL-PRUEFEN.
           IF W-EINGABE = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION UPPER-CASE(W-EINGABE) TO W-EINGABE
           IF (W-EINGABE(1:1) = "F" OR W-EINGABE(1:1) = "P")
              AND W-EINGABE(2:6) IS NUMERIC
               MOVE W-EINGABE TO KT-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK AND (SU-ART = SPACE OR SU-ART = KT-ART)
                   MOVE "WAHL" TO BW-RUECK
                   MOVE KT-NR TO BW-NR
                   MOVE "J" TO W-FERTIG
               ELSE
                   MOVE "Diese Nummer gibt es hier nicht." TO W-MELDUNG
               END-IF
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO W-NR
           IF FUNCTION TEST-NUMVAL(W-EINGABE) = 0
               COMPUTE W-NR = FUNCTION NUMVAL(W-EINGABE)
           END-IF
           IF W-NR >= 1 AND W-NR <= 15
              AND (W-SEITE - 1) * 15 + W-NR <= SU-ANZAHL
               COMPUTE W-I = (W-SEITE - 1) * 15 + W-NR
               MOVE "WAHL" TO BW-RUECK
               MOVE ST-NR(W-I) TO BW-NR
               MOVE "J" TO W-FERTIG
           ELSE
               MOVE "Bitte eine Zeile der Liste oder eine Nummer."
                 TO W-MELDUNG
           END-IF.

       COPY bildkopf-p.
