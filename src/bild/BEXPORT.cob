       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEXPORT.
      * Maske für den Export als CSV, vCard oder JSON.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY austausch.
       01  W-FORMAT                PIC X VALUE "C".
       01  W-DATEI                 PIC X(58).
       01  W-ARCHIV                PIC X VALUE "N".
       01  W-FERTIG                PIC X.
       SCREEN SECTION.
       01  S-EXPORT.
           COPY bildkopf.
           COPY befehl.
           05  LINE 3 COLUMN 2 VALUE "Format ..........:".
           05  LINE 3 COLUMN 21 PIC X USING W-FORMAT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 3 COLUMN 24 VALUE
               "C = CSV (Excel), V = vCard, J = JSON".
           05  LINE 5 COLUMN 2 VALUE "Datei ...........:".
           05  LINE 5 COLUMN 21 PIC X(58) USING W-DATEI
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 6 COLUMN 21 VALUE
               "leer = ~/adressen.csv, .vcf oder .json".
           05  LINE 8 COLUMN 2 VALUE "Mit Archiv ......:".
           05  LINE 8 COLUMN 21 PIC X USING W-ARCHIV
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 24 VALUE "J = archivierte mit exportieren".
           05  LINE 11 COLUMN 2 VALUE
               "CSV: je Kontakt Standardadresse und bevorzugte Wege.".
           05  LINE 12 COLUMN 2 VALUE
               "vCard und JSON: alle Adressen und Kommunikationswege.".
       PROCEDURE DIVISION.
           MOVE "Export" TO W-TITEL
           MOVE SPACES TO W-KOPF-RECHTS W-MELDUNG W-DATEI
           MOVE "Enter=Exportieren  Z=Zurück  ?=Hilfe"
             TO W-TASTEN1
           MOVE "?   1001Z   1003" TO W-BEF-TAB
           MOVE SPACES TO W-TASTEN2
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               DISPLAY S-EXPORT
               ACCEPT S-EXPORT
               MOVE SPACES TO W-MELDUNG
               PERFORM BEFEHL-AUSWERTEN
               MOVE FUNCTION UPPER-CASE(W-FORMAT) TO W-FORMAT
               MOVE FUNCTION UPPER-CASE(W-ARCHIV) TO W-ARCHIV
               EVALUATE TRUE
                   WHEN T-UNBEKANNT
                       CONTINUE
                   WHEN T-F3
                       MOVE "J" TO W-FERTIG
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-ENTER
                       PERFORM EXPORTIEREN
               END-EVALUATE
           END-PERFORM
           GOBACK.

       EXPORTIEREN.
           IF W-FORMAT NOT = "C" AND NOT = "V" AND NOT = "J"
               MOVE "Format: C, V oder J." TO W-MELDUNG
               EXIT PARAGRAPH
           END-IF
           INITIALIZE AU-PARM
           MOVE W-FORMAT TO AU-FORMAT
           MOVE W-ARCHIV TO AU-MIT-ARCHIV
           MOVE W-DATEI TO AU-DATEI
           IF W-DATEI = SPACES
               EVALUATE W-FORMAT
                   WHEN "C" MOVE "~/adressen.csv" TO AU-DATEI
                   WHEN "V" MOVE "~/adressen.vcf" TO AU-DATEI
                   WHEN "J" MOVE "~/adressen.json" TO AU-DATEI
               END-EVALUATE
           END-IF
           CALL "EXPORT" USING AU-PARM
           IF AU-ERFOLG
               STRING FUNCTION TRIM(AU-MELDUNG) " nach "
                      FUNCTION TRIM(AU-DATEI) DELIMITED BY SIZE
                 INTO W-MELDUNG
           ELSE
               MOVE AU-MELDUNG TO W-MELDUNG
           END-IF.

       COPY befehl-p.
       COPY bildkopf-p.
