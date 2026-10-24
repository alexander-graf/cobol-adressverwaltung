       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIMPORT.
      * Maske für den CSV-Import (Format wie der CSV-Export).
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY austausch.
       01  W-DATEI                 PIC X(58).
       01  W-FERTIG                PIC X.
       SCREEN SECTION.
       01  S-IMPORT.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE "CSV-Datei .......:".
           05  LINE 3 COLUMN 21 PIC X(58) USING W-DATEI
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 4 COLUMN 21 VALUE "z. B. ~/adressen.csv".
           05  LINE 7 COLUMN 2 VALUE
               "Erwartet wird das Format des CSV-Exports (Semikolon,".
           05  LINE 8 COLUMN 2 VALUE
               "UTF-8, erste Zeile Überschriften). Nummern werden neu".
           05  LINE 9 COLUMN 2 VALUE
               "vergeben, Dubletten übersprungen. Das Protokoll".
           05  LINE 10 COLUMN 2 VALUE
               "steht in ~/adrv-import-protokoll.txt.".
       PROCEDURE DIVISION.
           MOVE "Import" TO W-TITEL
           MOVE SPACES TO W-KOPF-RECHTS W-MELDUNG W-DATEI W-TASTEN2
           MOVE "Enter importieren  F3 zurück  F1 Hilfe" TO W-TASTEN1
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               DISPLAY S-IMPORT
               ACCEPT S-IMPORT
               MOVE SPACES TO W-MELDUNG
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "J" TO W-FERTIG
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-ENTER AND W-DATEI = SPACES
                       MOVE "Bitte eine Datei angeben." TO W-MELDUNG
                   WHEN T-ENTER
                       INITIALIZE AU-PARM
                       MOVE W-DATEI TO AU-DATEI
                       MOVE "~/adrv-import-protokoll.txt"
                         TO AU-PROTOKOLL
                       CALL "IMPORT" USING AU-PARM
                       MOVE AU-MELDUNG TO W-MELDUNG
               END-EVALUATE
           END-PERFORM
           GOBACK.

       COPY bildkopf-p.
