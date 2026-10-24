       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHILFE.
      * Hilfeseite mit allen Tasten.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS W-TASTE
           CURSOR IS W-CURSOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY tasten.
       01  W-DUMMY                 PIC X.
       SCREEN SECTION.
       01  S-HILFE.
           COPY bildkopf.
           05  LINE 3 COLUMN 3 VALUE "Tasten" HIGHLIGHT.
           05  LINE 4 COLUMN 3 VALUE
               "Tab / Pfeile  Feld wechseln     Enter  bestätigen".
           05  LINE 5 COLUMN 3 VALUE
               "F2  Speichern                   F3/Esc zurück".
           05  LINE 6 COLUMN 3 VALUE
               "F4  Auswahlliste zum Feld unter dem Cursor".
           05  LINE 7 COLUMN 3 VALUE
               "F5  Neu (in Listen)             F6  zurück zur Suche".
           05  LINE 8 COLUMN 3 VALUE
               "F7  Anschrift und Briefanrede   F8  Ansprechpartner".
           05  LINE 9 COLUMN 3 VALUE
               "F9  Adressen                    F10 Kommunikation".
           05  LINE 10 COLUMN 3 VALUE
               "F11 endgültig löschen (nur archivierte Kontakte)".
           05  LINE 11 COLUMN 3 VALUE
               "F12 archivieren / wieder aktivieren".
           05  LINE 12 COLUMN 3 VALUE
               "Bild auf/ab oder F7/F8 blättern in Listen".
           05  LINE 14 COLUMN 3 VALUE "Suche" HIGHLIGHT.
           05  LINE 15 COLUMN 3 VALUE
               "mueller findet Müller (Anfang des Namens),".
           05  LINE 16 COLUMN 3 VALUE
               "*bau findet alle, deren Name bau enthält.".
           05  LINE 17 COLUMN 3 VALUE
               "Personen werden nach Nachname, dann Vorname gesucht.".
           05  LINE 19 COLUMN 3 VALUE
               "Datum immer als TT.MM.JJJJ, Land als ISO-Code (DE).".
       PROCEDURE DIVISION.
           MOVE "Hilfe" TO W-TITEL
           MOVE SPACES TO W-KOPF-RECHTS W-TASTEN2 W-MELDUNG
           MOVE "Enter / F3  zurück" TO W-TASTEN1
           PERFORM KOPF-BAUEN
           DISPLAY S-HILFE
           ACCEPT W-DUMMY LINE 24 COLUMN 78
           GOBACK.

       COPY bildkopf-p.
