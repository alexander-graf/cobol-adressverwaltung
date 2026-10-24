       IDENTIFICATION DIVISION.
       PROGRAM-ID. BHILFE.
      * Hilfeseite mit allen Befehlen und Tasten.
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
           05  LINE 3 COLUMN 3 VALUE
               "Befehle" HIGHLIGHT.
           05  LINE 3 COLUMN 22 VALUE
               "in Befehl ===> oder ins Auswahlfeld tippen, Enter".
           05  LINE 4 COLUMN 3 VALUE
               "?   Hilfe                 F1".
           05  LINE 4 COLUMN 42 VALUE
               "S   Speichern             F2".
           05  LINE 5 COLUMN 3 VALUE
               "Z   zurück, auch Esc      F3".
           05  LINE 5 COLUMN 42 VALUE
               "?   im Codefeld: Liste    F4".
           05  LINE 6 COLUMN 3 VALUE
               "N   neu                   F5".
           05  LINE 6 COLUMN 42 VALUE
               "SU  zurück zur Suche      F6".
           05  LINE 7 COLUMN 3 VALUE
               "AN  Anschrift, Anrede     F7".
           05  LINE 7 COLUMN 42 VALUE
               "AP  Ansprechpartner       F8".
           05  LINE 8 COLUMN 3 VALUE
               "A   Adressen              F9".
           05  LINE 8 COLUMN 42 VALUE
               "K   Kommunikation         F10".
           05  LINE 9 COLUMN 3 VALUE
               "L   löschen           F11/F12".
           05  LINE 9 COLUMN 42 VALUE
               "AR  archivieren/aktivieren F12".
           05  LINE 10 COLUMN 3 VALUE
               "E   Etiketten der Treffer F11".
           05  LINE 10 COLUMN 42 VALUE
               "+ / -  blättern    Bild ab/auf".
           05  LINE 11 COLUMN 3 VALUE
               "Die F-Tasten gehen auch mit Shift (Shift+F2 = F2).".
           05  LINE 12 COLUMN 3 VALUE
               "Endgültig löschen nur bei archivierten Kontakten.".
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
           MOVE "Enter = zurück" TO W-TASTEN1
           PERFORM KOPF-BAUEN
           DISPLAY S-HILFE
           ACCEPT W-DUMMY LINE 24 COLUMN 78
           GOBACK.

       COPY bildkopf-p.
