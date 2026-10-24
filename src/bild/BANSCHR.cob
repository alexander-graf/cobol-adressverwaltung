       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANSCHR.
      * Zeigt Anschrift (DIN 5008) und Briefanrede eines Kontakts und
      * schreibt beides nach ~/adrv-anschrift.txt zum Weiterverwenden.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY suche.
       COPY austausch.
       01  W-DUMMY                 PIC X.
       LINKAGE SECTION.
       01  BN-NR                   PIC X(7).
       SCREEN SECTION.
       01  S-ANSCHRIFT.
           COPY bildkopf.
           05  LINE 3 COLUMN 3 VALUE "Anschrift" HIGHLIGHT.
           05  LINE 5 COLUMN 5 PIC X(60) FROM DI-ZEILE(1).
           05  LINE 6 COLUMN 5 PIC X(60) FROM DI-ZEILE(2).
           05  LINE 7 COLUMN 5 PIC X(60) FROM DI-ZEILE(3).
           05  LINE 8 COLUMN 5 PIC X(60) FROM DI-ZEILE(4).
           05  LINE 9 COLUMN 5 PIC X(60) FROM DI-ZEILE(5).
           05  LINE 10 COLUMN 5 PIC X(60) FROM DI-ZEILE(6).
           05  LINE 13 COLUMN 3 VALUE "Briefanrede" HIGHLIGHT.
           05  LINE 15 COLUMN 5 PIC X(74) FROM DI-BRIEFANR.
       PROCEDURE DIVISION USING BN-NR.
           MOVE BN-NR TO KT-NR
           MOVE "LESEN" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           IF NOT IO-OK
               GOBACK
           END-IF
           MOVE "ANSCHR" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           INITIALIZE SU-PARM
           MOVE 1 TO SU-ANZAHL
           MOVE KT-NR TO ST-NR(1)
           INITIALIZE AU-PARM
           MOVE "~/adrv-anschrift.txt" TO AU-DATEI
           MOVE "J" TO AU-MIT-ANREDE
           CALL "ETIKETT" USING SU-PARM AU-PARM
           IF AU-ERFOLG
               MOVE "Auch gespeichert in ~/adrv-anschrift.txt"
                 TO W-MELDUNG
           ELSE
               MOVE AU-MELDUNG TO W-MELDUNG
           END-IF
           MOVE SPACES TO W-TITEL W-KOPF-RECHTS W-TASTEN2
           STRING "Anschrift " KT-NR DELIMITED BY SIZE INTO W-TITEL
           MOVE "Enter = zurück" TO W-TASTEN1
           PERFORM KOPF-BAUEN
           DISPLAY S-ANSCHRIFT
           ACCEPT W-DUMMY LINE 24 COLUMN 78
           GOBACK.

       COPY bildkopf-p.
