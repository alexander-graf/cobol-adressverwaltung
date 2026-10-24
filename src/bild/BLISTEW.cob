       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLISTEW.
      * Auswahl aus einer Werteliste (Modul CODES). Liefert den Code
      * in LW-WAHL; bei F3 bleibt LW-WAHL leer.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY codes.
       01  W-EINGABE               PIC XX.
       01  W-NR                    PIC 99.
       01  W-I                     PIC 99.
       01  W-ZEILE                 PIC 99.
       01  W-ANZEIGE               PIC X(60).
       01  W-ZL                    PIC Z9.
       01  W-FERTIG                PIC X.
       LINKAGE SECTION.
       01  LW-LISTE                PIC X(8).
       01  LW-TITEL                PIC X(50).
       01  LW-WAHL                 PIC X(20).
       SCREEN SECTION.
       01  S-RAHMEN.
           COPY bildkopf.
       01  S-EINGABE.
           05  LINE 21 COLUMN 3 VALUE "Nummer:".
           05  LINE 21 COLUMN 11 PIC XX USING W-EINGABE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
       PROCEDURE DIVISION USING LW-LISTE LW-TITEL LW-WAHL.
           MOVE SPACES TO LW-WAHL W-MELDUNG W-KOPF-RECHTS W-TASTEN2
           MOVE "LISTE" TO CD-FUNKTION
           MOVE LW-LISTE TO CD-LISTE
           CALL "CODES" USING CD-PARM
           MOVE LW-TITEL TO W-TITEL
           MOVE "Nummer eingeben, Enter   Z=Abbrechen" TO W-TASTEN1
           MOVE "Z   1003" TO W-BEF-TAB
           MOVE "J" TO W-BEF-LISTE
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               DISPLAY S-RAHMEN
               PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > CD-ANZAHL
                   MOVE W-I TO W-ZL
                   MOVE SPACES TO W-ANZEIGE
                   STRING W-ZL "  " CD-E-CODE(W-I) " "
                          CD-E-TEXT(W-I) DELIMITED BY SIZE
                     INTO W-ANZEIGE
                   COMPUTE W-ZEILE = W-I + 2
                   DISPLAY W-ANZEIGE LINE W-ZEILE COLUMN 3
               END-PERFORM
               MOVE SPACES TO W-EINGABE
               ACCEPT S-EINGABE
               MOVE SPACES TO W-MELDUNG
               MOVE W-EINGABE TO W-BEFEHL
               PERFORM BEFEHL-AUSWERTEN
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "J" TO W-FERTIG
                   WHEN OTHER
                       PERFORM NUMMER-PRUEFEN
               END-EVALUATE
           END-PERFORM
           GOBACK.

       NUMMER-PRUEFEN.
           MOVE 0 TO W-NR
           IF W-EINGABE NOT = SPACES
              AND FUNCTION TEST-NUMVAL(W-EINGABE) = 0
               COMPUTE W-NR = FUNCTION NUMVAL(W-EINGABE)
           END-IF
           IF W-NR >= 1 AND W-NR <= CD-ANZAHL
               MOVE CD-E-CODE(W-NR) TO LW-WAHL
               MOVE "J" TO W-FERTIG
           ELSE
               MOVE "Bitte eine Nummer aus der Liste eingeben."
                 TO W-MELDUNG
           END-IF.

       COPY befehl-p.
       COPY bildkopf-p.
