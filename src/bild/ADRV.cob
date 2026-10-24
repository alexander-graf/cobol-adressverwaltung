       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADRV.
      * Hauptprogramm der Adressverwaltung: Hauptmenü.
      * Start über das Skript adrv (luit, Latin-9, Tastenvariablen).
      * Ohne Bildschirm für Skripte:
      *   adrv --import datei.csv
      *   adrv --export C|V|J datei
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
       COPY adresse.
       COPY komm.
       01  W-WAHL                  PIC X.
       01  W-ENDE                  PIC X VALUE "N".
       01  W-ART                   PIC X.
       01  W-ARCHIV                PIC X.
       COPY austausch.
       COPY zeichen.
       01  W-ARGUMENTE             PIC X(300).
       01  W-ARG1                  PIC X(20).
       01  W-ARG2                  PIC X(200).
       01  W-ARG3                  PIC X(200).
       SCREEN SECTION.
       01  S-MENUE.
           COPY bildkopf.
           05  LINE 4 COLUMN 10 VALUE "1   Firmen".
           05  LINE 5 COLUMN 10 VALUE "2   Personen".
           05  LINE 6 COLUMN 10 VALUE "3   Suche über alle Kontakte".
           05  LINE 8 COLUMN 10 VALUE "4   Geburtstage".
           05  LINE 9 COLUMN 10 VALUE "5   Export (CSV, vCard, JSON)".
           05  LINE 10 COLUMN 10 VALUE "6   Import (CSV)".
           05  LINE 12 COLUMN 10 VALUE "7   Archiv".
           05  LINE 14 COLUMN 10 VALUE "0   Ende".
           05  LINE 17 COLUMN 10 VALUE "Auswahl:".
           05  LINE 17 COLUMN 19 PIC X USING W-WAHL
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
       PROCEDURE DIVISION.
           ACCEPT W-ARGUMENTE FROM COMMAND-LINE
           IF W-ARGUMENTE NOT = SPACES
               PERFORM KOMMANDOZEILE
               STOP RUN
           END-IF
      *    Ohne diesen Aufruf zeigt ncurses Umlaute als Leerzeichen;
      *    0 ist LC_CTYPE (glibc), "" heißt: aus der Umgebung.
           CALL "setlocale" USING BY VALUE 0 BY CONTENT X"00"
           MOVE "Hauptmenü" TO W-TITEL
           MOVE "F1 Hilfe   F3 Ende" TO W-TASTEN1
           MOVE SPACES TO W-TASTEN2 W-MELDUNG W-KOPF-RECHTS
           PERFORM UNTIL W-ENDE = "J"
               PERFORM KOPF-BAUEN
               MOVE SPACE TO W-WAHL
               DISPLAY S-MENUE
               ACCEPT S-MENUE
               MOVE SPACES TO W-MELDUNG
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "J" TO W-ENDE
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN OTHER
                       PERFORM WAHL-AUSFUEHREN
               END-EVALUATE
           END-PERFORM
           MOVE "SCHLIESS" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           CALL "ADIO" USING IO-PARM AD-SATZ
           CALL "KOIO" USING IO-PARM KO-SATZ
           MOVE 0 TO RETURN-CODE
           STOP RUN.

       WAHL-AUSFUEHREN.
           MOVE "N" TO W-ARCHIV
           EVALUATE W-WAHL
               WHEN "1"
                   MOVE "F" TO W-ART
                   CALL "BSUCHE" USING W-ART W-ARCHIV
               WHEN "2"
                   MOVE "P" TO W-ART
                   CALL "BSUCHE" USING W-ART W-ARCHIV
               WHEN "3"
                   MOVE SPACE TO W-ART
                   CALL "BSUCHE" USING W-ART W-ARCHIV
               WHEN "4"
                   CALL "BGEB"
               WHEN "5"
                   CALL "BEXPORT"
               WHEN "6"
                   CALL "BIMPORT"
               WHEN "7"
                   MOVE SPACE TO W-ART
                   MOVE "J" TO W-ARCHIV
                   CALL "BSUCHE" USING W-ART W-ARCHIV
               WHEN "0"
                   MOVE "J" TO W-ENDE
               WHEN OTHER
                   MOVE "Bitte 0 bis 7 wählen." TO W-MELDUNG
           END-EVALUATE.

      * Import und Export ohne Bildschirm. Die Meldung wird nach UTF-8
      * gewandelt, weil hier kein luit dazwischen sitzt.
       KOMMANDOZEILE.
           UNSTRING W-ARGUMENTE DELIMITED BY ALL SPACE
               INTO W-ARG1 W-ARG2 W-ARG3
           INITIALIZE AU-PARM
           EVALUATE W-ARG1
               WHEN "--import"
                   MOVE W-ARG2 TO AU-DATEI
                   MOVE "import-protokoll.txt" TO AU-PROTOKOLL
                   CALL "IMPORT" USING AU-PARM
               WHEN "--export"
                   MOVE W-ARG2 TO AU-FORMAT
                   MOVE W-ARG3 TO AU-DATEI
                   CALL "EXPORT" USING AU-PARM
               WHEN OTHER
                   MOVE "N" TO AU-OK
                   MOVE "Aufruf: adrv [--import datei.csv | --export "
                      & "C|V|J datei]" TO AU-MELDUNG
           END-EVALUATE
           MOVE "U" TO ZC-RICHTUNG
           MOVE AU-MELDUNG TO ZC-EIN
           MOVE FUNCTION LENGTH(FUNCTION TRIM(AU-MELDUNG TRAILING))
             TO ZC-EIN-LEN
           CALL "ZEICHEN" USING ZC-PARM
           DISPLAY ZC-AUS(1:ZC-AUS-LEN)
           MOVE "SCHLIESS" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           CALL "ADIO" USING IO-PARM AD-SATZ
           CALL "KOIO" USING IO-PARM KO-SATZ
           IF AU-ERFOLG
               MOVE 0 TO RETURN-CODE
           ELSE
               MOVE 1 TO RETURN-CODE
           END-IF.

       COPY bildkopf-p.
