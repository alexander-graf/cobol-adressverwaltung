       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSUCHE.
      * Suchmaske. Nach Enter zeigt BWAHL die Treffer; ein gewählter
      * Kontakt öffnet sich in BFIRMA oder BPERSON. Danach geht es
      * zurück in die (neu gesuchte) Liste.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY suche.
       COPY bwahl.
       COPY austausch.
       01  W-RUECK                 PIC X(8).
       01  W-NR                    PIC X(7).
       01  W-KEINE-FIRMA           PIC X(7) VALUE SPACES.
       01  W-LISTE                 PIC X(8).
       01  W-LISTE-TITEL           PIC X(50).
       01  W-WAHL                  PIC X(20).
       01  W-FERTIG                PIC X.
       01  W-LISTE-FERTIG          PIC X.
       LINKAGE SECTION.
       01  BS-ART                  PIC X.
       01  BS-ARCHIV               PIC X.
       SCREEN SECTION.
       01  S-SUCHE.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE "Suchbegriff .....:".
           05  LINE 3 COLUMN 21 PIC X(30) USING SU-BEGRIFF
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 4 COLUMN 21 VALUE
               "Anfang des Namens; *teil sucht im ganzen Namen".
           05  LINE 6 COLUMN 2 VALUE "Art .............:".
           05  LINE 6 COLUMN 21 PIC X USING SU-ART
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 6 COLUMN 24 VALUE
               "F = Firmen, P = Personen, leer = alle".
           05  LINE 7 COLUMN 2 VALUE "PLZ beginnt mit .:".
           05  LINE 7 COLUMN 21 PIC X(10) USING SU-PLZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 2 VALUE "Ort beginnt mit .:".
           05  LINE 8 COLUMN 21 PIC X(30) USING SU-ORT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 2 VALUE "Kategorie .......:".
           05  LINE 9 COLUMN 21 PIC XX USING SU-KATEGORIE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 24 VALUE "F4 = Liste".
           05  LINE 10 COLUMN 2 VALUE "Schlagwort ......:".
           05  LINE 10 COLUMN 21 PIC X(11) USING SU-SCHLAGWORT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 11 COLUMN 2 VALUE "Archiv ..........:".
           05  LINE 11 COLUMN 21 PIC X USING SU-ARCHIV
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 11 COLUMN 24 VALUE
               "J = nur archivierte Kontakte".
       PROCEDURE DIVISION USING BS-ART BS-ARCHIV.
           INITIALIZE SU-PARM
           MOVE BS-ART TO SU-ART
           MOVE BS-ARCHIV TO SU-ARCHIV
           MOVE SPACES TO W-MELDUNG W-KOPF-RECHTS
           EVALUATE BS-ART
               WHEN "F" MOVE "Firmen suchen" TO W-TITEL
               WHEN "P" MOVE "Personen suchen" TO W-TITEL
               WHEN OTHER MOVE "Kontakte suchen" TO W-TITEL
           END-EVALUATE
           IF BS-ARCHIV = "J"
               MOVE "Archiv" TO W-TITEL
           END-IF
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               MOVE "Enter suchen  F3 zurück  F4 Liste  F5 neu"
                 TO W-TASTEN1
               MOVE "Leere Suche zeigt alle.  F1 Hilfe" TO W-TASTEN2
               PERFORM KOPF-BAUEN
               DISPLAY S-SUCHE
               ACCEPT S-SUCHE
               MOVE SPACES TO W-MELDUNG
               MOVE FUNCTION UPPER-CASE(SU-ART) TO SU-ART
               MOVE FUNCTION UPPER-CASE(SU-ARCHIV) TO SU-ARCHIV
               MOVE FUNCTION UPPER-CASE(SU-KATEGORIE) TO SU-KATEGORIE
               EVALUATE TRUE
                   WHEN T-F3
                       MOVE "J" TO W-FERTIG
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-F4
                       IF W-CUR-ZEILE = 9
                           MOVE "KATEG" TO W-LISTE
                           MOVE "Kategorie wählen" TO W-LISTE-TITEL
                           CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL
                                                W-WAHL
                           IF W-WAHL NOT = SPACES
                               MOVE W-WAHL TO SU-KATEGORIE
                           END-IF
                       ELSE
                           MOVE "F4 gibt es hier nur bei Kategorie."
                             TO W-MELDUNG
                       END-IF
                   WHEN T-F5
                       PERFORM NEU-ANLEGEN
                   WHEN T-ENTER
                       PERFORM LISTE-ZEIGEN
               END-EVALUATE
           END-PERFORM
           GOBACK.

       LISTE-ZEIGEN.
           MOVE W-TITEL TO BW-TITEL
           MOVE "S" TO BW-MODUS
           MOVE SPACES TO BW-MELDUNG
           MOVE "N" TO W-LISTE-FERTIG
           PERFORM UNTIL W-LISTE-FERTIG = "J"
               CALL "BWAHL" USING SU-PARM BW-PARM
               EVALUATE BW-RUECK
                   WHEN "WAHL"
                       MOVE BW-NR TO W-NR
                       PERFORM KONTAKT-OEFFNEN
                       IF W-RUECK = "SUCH"
                           MOVE "J" TO W-LISTE-FERTIG
                       END-IF
                   WHEN "NEU"
                       PERFORM NEU-ANLEGEN
                       MOVE W-MELDUNG TO BW-MELDUNG
                       MOVE SPACES TO W-MELDUNG
                   WHEN "ETIK"
                       PERFORM ETIKETTEN-SCHREIBEN
                   WHEN OTHER
                       MOVE "J" TO W-LISTE-FERTIG
               END-EVALUATE
           END-PERFORM.

       KONTAKT-OEFFNEN.
           MOVE SPACES TO W-RUECK
           IF W-NR(1:1) = "F"
               CALL "BFIRMA" USING W-NR W-RUECK
           ELSE
               CALL "BPERSON" USING W-NR W-RUECK W-KEINE-FIRMA
           END-IF.

       NEU-ANLEGEN.
           MOVE SPACES TO W-NR W-RUECK
           EVALUATE SU-ART
               WHEN "F"
                   CALL "BFIRMA" USING W-NR W-RUECK
               WHEN "P"
                   CALL "BPERSON" USING W-NR W-RUECK W-KEINE-FIRMA
               WHEN OTHER
                   MOVE "Für Neu bitte bei Art F oder P angeben."
                     TO W-MELDUNG
           END-EVALUATE.

       ETIKETTEN-SCHREIBEN.
           INITIALIZE AU-PARM
           MOVE "~/adrv-etiketten.txt" TO AU-DATEI
           MOVE "N" TO AU-MIT-ANREDE
           CALL "ETIKETT" USING SU-PARM AU-PARM
           IF AU-ERFOLG
               STRING FUNCTION TRIM(AU-MELDUNG) " nach "
                      FUNCTION TRIM(AU-DATEI) DELIMITED BY SIZE
                 INTO BW-MELDUNG
           ELSE
               MOVE AU-MELDUNG TO BW-MELDUNG
           END-IF.

       COPY bildkopf-p.
