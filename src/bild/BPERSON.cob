       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPERSON.
      * Maske Person. BP-NR leer = neue Person; BP-FIRMA ordnet eine
      * neue Person gleich einer Firma zu. BP-RUECK "SUCH" = zurück
      * zur Suche. Ruft BFIRMA nicht auf (sonst Aufrufzyklus).
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
       COPY kontakt REPLACING LEADING ==KT-== BY ==WK-==
                              LEADING ==KF-== BY ==WF-==
                              LEADING ==KP-== BY ==WP-==.
       COPY adresse.
       COPY komm.
       COPY codes.
       COPY pruef.
       COPY suche.
       COPY bwahl.
       COPY wort-d.
       01  W-ORIG                  PIC X(781).
       01  W-GEB-TEXT              PIC X(10).
       01  W-GEB-ORIG              PIC X(10).
       01  W-FERTIG                PIC X.
       01  W-FEHLER                PIC X.
       01  W-ANREDE-TEXT           PIC X(30).
       01  W-KAT-TEXT              PIC X(30).
       01  W-FIRMA-TEXT            PIC X(50).
       01  W-BRIEF-TEXT            PIC X(80).
       01  W-STDADR-TEXT           PIC X(60).
       01  W-KOMM-TEXT             PIC X(60).
       01  W-KURZNAME              PIC X(40).
       01  W-LISTE                 PIC X(8).
       01  W-LISTE-TITEL           PIC X(50).
       01  W-WAHL                  PIC X(20).
       LINKAGE SECTION.
       01  BP-NR                   PIC X(7).
       01  BP-RUECK                PIC X(8).
       01  BP-FIRMA                PIC X(7).
       SCREEN SECTION.
       01  S-PERSON.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE "Anrede ..........:".
           05  LINE 3 COLUMN 21 PIC X USING KP-ANREDE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 3 COLUMN 24 PIC X(30) FROM W-ANREDE-TEXT.
           05  LINE 3 COLUMN 57 VALUE "F4 = Liste".
           05  LINE 4 COLUMN 2 VALUE "Titel ...........:".
           05  LINE 4 COLUMN 21 PIC X(20) USING KP-TITEL
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 5 COLUMN 2 VALUE "Vorname .........:".
           05  LINE 5 COLUMN 21 PIC X(30) USING KP-VORNAME
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 6 COLUMN 2 VALUE "Weitere Vornamen :".
           05  LINE 6 COLUMN 21 PIC X(30) USING KP-VORNAMEN2
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 7 COLUMN 2 VALUE "Namensvorsatz ...:".
           05  LINE 7 COLUMN 21 PIC X(20) USING KP-VORSATZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 7 COLUMN 44 VALUE "Namenszusatz:".
           05  LINE 7 COLUMN 58 PIC X(10) USING KP-ZUSATZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 2 VALUE "Nachname ........:".
           05  LINE 8 COLUMN 21 PIC X(40) USING KP-NACHNAME
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 2 VALUE "Geburtsdatum ....:".
           05  LINE 9 COLUMN 21 PIC X(10) USING W-GEB-TEXT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 33 VALUE "TT.MM.JJJJ".
           05  LINE 10 COLUMN 2 VALUE "Firma ...........:".
           05  LINE 10 COLUMN 21 PIC X(7) USING KT-FIRMA-NR
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 10 COLUMN 30 PIC X(50) FROM W-FIRMA-TEXT.
           05  LINE 11 COLUMN 2 VALUE "Position ........:".
           05  LINE 11 COLUMN 21 PIC X(40) USING KP-POSITION
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 2 VALUE "Abteilung .......:".
           05  LINE 12 COLUMN 21 PIC X(40) USING KP-ABTEILUNG
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 13 COLUMN 2 VALUE "Kategorie .......:".
           05  LINE 13 COLUMN 21 PIC XX USING KT-KATEGORIE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 13 COLUMN 25 PIC X(30) FROM W-KAT-TEXT.
           05  LINE 13 COLUMN 57 VALUE "F4 = Liste".
           05  LINE 14 COLUMN 2 VALUE "Schlagwörter ....:".
           05  LINE 14 COLUMN 21 PIC X(11) USING KT-SCHLAGWORT(1)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 14 COLUMN 33 PIC X(11) USING KT-SCHLAGWORT(2)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 14 COLUMN 45 PIC X(11) USING KT-SCHLAGWORT(3)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 14 COLUMN 57 PIC X(11) USING KT-SCHLAGWORT(4)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 14 COLUMN 69 PIC X(11) USING KT-SCHLAGWORT(5)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 15 COLUMN 2 VALUE "Notiz ...........:".
           05  LINE 15 COLUMN 21 PIC X(60) USING KT-NOTIZ(1)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 16 COLUMN 21 PIC X(60) USING KT-NOTIZ(2)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 17 COLUMN 21 PIC X(60) USING KT-NOTIZ(3)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 18 COLUMN 21 PIC X(60) USING KT-NOTIZ(4)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 19 COLUMN 2 VALUE "Briefanrede .....:".
           05  LINE 19 COLUMN 21 PIC X(60) FROM W-BRIEF-TEXT.
           05  LINE 20 COLUMN 2 VALUE "Standardadresse .:".
           05  LINE 20 COLUMN 21 PIC X(60) FROM W-STDADR-TEXT.
           05  LINE 21 COLUMN 2 VALUE "Kommunikation ...:".
           05  LINE 21 COLUMN 21 PIC X(60) FROM W-KOMM-TEXT.
       PROCEDURE DIVISION USING BP-NR BP-RUECK BP-FIRMA.
           MOVE SPACES TO BP-RUECK W-MELDUNG W-BESTAETIGEN
           MOVE "F2 Speichern  F3 zurück  F4 Liste/Firma wählen  "
              & "F6 Suche  F7 Anschrift" TO W-TASTEN1
           MOVE "F9 Adressen  F10 Kommunikation  F11 Löschen  "
              & "F12 Archiv  F1 Hilfe" TO W-TASTEN2
           IF BP-NR = SPACES
               INITIALIZE KT-SATZ
               MOVE "P" TO KT-ART
               MOVE "N" TO KT-ARCHIV
               MOVE "K" TO KP-ANREDE
               MOVE "IN" TO KT-KATEGORIE
               MOVE BP-FIRMA TO KT-FIRMA-NR
               IF BP-FIRMA NOT = SPACES
                   MOVE "KU" TO KT-KATEGORIE
               END-IF
           ELSE
               MOVE BP-NR TO KT-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF NOT IO-OK
                   GOBACK
               END-IF
           END-IF
           MOVE "DATTEXT" TO PR-FUNKTION
           MOVE KP-GEBDATUM TO PR-DATUM
           CALL "PRUEF" USING PR-PARM
           MOVE PR-WERT TO W-GEB-TEXT
           MOVE W-GEB-TEXT TO W-GEB-ORIG
           MOVE KT-SATZ TO W-ORIG
           PERFORM INFO-LADEN
           MOVE 0321 TO W-CURSOR
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               PERFORM KOPF-BAUEN
               DISPLAY S-PERSON
               ACCEPT S-PERSON
               MOVE SPACES TO W-MELDUNG
               PERFORM TASTE-AUSWERTEN
           END-PERFORM
           GOBACK.

       TASTE-AUSWERTEN.
           IF NOT (T-F2 AND W-BESTAETIGEN = "DUBL")
              AND NOT (T-F3 AND W-BESTAETIGEN = "VERWERF")
              AND NOT (T-F6 AND W-BESTAETIGEN = "VERWERF")
              AND NOT (T-F11 AND W-BESTAETIGEN = "TILGEN")
               MOVE SPACES TO W-BESTAETIGEN
           END-IF
           EVALUATE TRUE
               WHEN T-F1
                   CALL "BHILFE"
               WHEN T-F2
                   PERFORM SPEICHERN
               WHEN T-F3
                   PERFORM VERLASSEN
               WHEN T-F6
                   PERFORM VERLASSEN
                   IF W-FERTIG = "J"
                       MOVE "SUCH" TO BP-RUECK
                   END-IF
               WHEN T-F4
                   PERFORM AUSWAHLLISTE
               WHEN T-ENTER
                   PERFORM PRUEFEN
                   IF W-FEHLER = "N"
                       MOVE "Eingaben in Ordnung. F2 speichert."
                         TO W-MELDUNG
                   END-IF
               WHEN KT-NR = SPACES
                   MOVE "Bitte erst speichern (F2)." TO W-MELDUNG
               WHEN T-F7
                   CALL "BANSCHR" USING KT-NR
               WHEN T-F9
                   PERFORM KURZNAME-BILDEN
                   CALL "BADRESS" USING KT-NR W-KURZNAME
                   PERFORM INFO-LADEN
               WHEN T-F10
                   PERFORM KURZNAME-BILDEN
                   CALL "BKOMM" USING KT-NR W-KURZNAME
                   PERFORM INFO-LADEN
               WHEN T-F11
                   PERFORM TILGEN
               WHEN T-F12
                   PERFORM ARCHIV-UMSCHALTEN
           END-EVALUATE.

       PRUEFEN.
           MOVE "N" TO W-FEHLER
           MOVE FUNCTION UPPER-CASE(KT-KATEGORIE) TO KT-KATEGORIE
           MOVE FUNCTION UPPER-CASE(KP-ANREDE) TO KP-ANREDE
           MOVE FUNCTION UPPER-CASE(KT-FIRMA-NR) TO KT-FIRMA-NR
           IF KP-ANREDE = SPACE
               MOVE "K" TO KP-ANREDE
           END-IF
           PERFORM INFO-LADEN
           EVALUATE TRUE
               WHEN KP-NACHNAME = SPACES
                   MOVE "Der Nachname fehlt." TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0821 TO W-CURSOR
               WHEN W-ANREDE-TEXT = SPACES
                   MOVE "Anrede: H, F, N oder K (F4 zeigt die Liste)."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0321 TO W-CURSOR
               WHEN W-KAT-TEXT = SPACES
                   MOVE "Kategorie unbekannt (F4 zeigt die Liste)."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 1321 TO W-CURSOR
               WHEN KT-FIRMA-NR NOT = SPACES
                AND W-FIRMA-TEXT(1:1) = "("
                   MOVE "Diese Firmennummer gibt es nicht (F4 sucht)."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 1021 TO W-CURSOR
           END-EVALUATE
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           MOVE "GEBDAT" TO PR-FUNKTION
           MOVE W-GEB-TEXT TO PR-WERT
           CALL "PRUEF" USING PR-PARM
           IF PR-GUELTIG
               MOVE PR-DATUM TO KP-GEBDATUM
           ELSE
               MOVE "J" TO W-FEHLER
               MOVE PR-MELDUNG TO W-MELDUNG
               MOVE 0921 TO W-CURSOR
           END-IF.

       SPEICHERN.
           PERFORM PRUEFEN
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           IF KT-NR = SPACES
               IF W-BESTAETIGEN NOT = "DUBL"
                   MOVE SPACES TO DI-PLZ
                   MOVE "DUBLETTE" TO DI-FUNKTION
                   PERFORM DIENST
                   IF DI-TREFFER-NR NOT = SPACES
                       STRING FUNCTION TRIM(DI-MELDUNG)
                              " - F2 erneut speichert trotzdem."
                              DELIMITED BY SIZE INTO W-MELDUNG
                       MOVE "DUBL" TO W-BESTAETIGEN
                       EXIT PARAGRAPH
                   END-IF
               END-IF
               MOVE SPACES TO W-BESTAETIGEN
               MOVE "NEU" TO DI-FUNKTION
           ELSE
               MOVE "SPEICHER" TO DI-FUNKTION
           END-IF
           PERFORM DIENST
           IF DI-ERFOLG
               MOVE KT-SATZ TO W-ORIG
               MOVE W-GEB-TEXT TO W-GEB-ORIG
               STRING "Gespeichert als " KT-NR "."
                      DELIMITED BY SIZE INTO W-MELDUNG
               PERFORM INFO-LADEN
           END-IF.

       VERLASSEN.
           IF (KT-SATZ = W-ORIG AND W-GEB-TEXT = W-GEB-ORIG)
              OR W-BESTAETIGEN = "VERWERF"
               MOVE "J" TO W-FERTIG
           ELSE
               MOVE "Nicht gespeichert! Taste erneut = verwerfen, "
                  & "F2 = speichern." TO W-MELDUNG
               MOVE "VERWERF" TO W-BESTAETIGEN
           END-IF.

       AUSWAHLLISTE.
           EVALUATE W-CUR-ZEILE
               WHEN 3
                   MOVE "ANREDE" TO W-LISTE
                   MOVE "Anrede wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO KP-ANREDE
                   END-IF
               WHEN 10
                   PERFORM FIRMA-WAEHLEN
               WHEN 13
                   MOVE "KATEG" TO W-LISTE
                   MOVE "Kategorie wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO KT-KATEGORIE
                   END-IF
               WHEN OTHER
                   MOVE "F4 gibt es bei Anrede, Firma und Kategorie."
                     TO W-MELDUNG
           END-EVALUATE
           PERFORM INFO-LADEN.

      * Firmenliste; ein Text im Firmenfeld dient als Suchbegriff.
       FIRMA-WAEHLEN.
           INITIALIZE SU-PARM
           MOVE "F" TO SU-ART
           IF KT-FIRMA-NR(2:6) IS NOT NUMERIC
               MOVE KT-FIRMA-NR TO SU-BEGRIFF
           END-IF
           MOVE "Firma zuordnen" TO BW-TITEL
           MOVE "A" TO BW-MODUS
           MOVE SPACES TO BW-MELDUNG
           CALL "BWAHL" USING SU-PARM BW-PARM
           IF BW-RUECK = "WAHL"
               MOVE BW-NR TO KT-FIRMA-NR
           END-IF.

       TILGEN.
           IF NOT KT-ARCHIVIERT
               MOVE "Endgültig löschen geht nur bei archivierten "
                  & "Kontakten (erst F12)." TO W-MELDUNG
               EXIT PARAGRAPH
           END-IF
           IF W-BESTAETIGEN NOT = "TILGEN"
               MOVE "Person samt Adressen und Kommunikation "
                  & "endgültig löschen? F11 erneut = ja" TO W-MELDUNG
               MOVE "TILGEN" TO W-BESTAETIGEN
               EXIT PARAGRAPH
           END-IF
           MOVE "TILGEN" TO DI-FUNKTION
           PERFORM DIENST
           IF DI-ERFOLG
               MOVE "J" TO W-FERTIG
           END-IF.

       ARCHIV-UMSCHALTEN.
           PERFORM PRUEFEN
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           IF KT-ARCHIVIERT
               MOVE "REAKTIV" TO DI-FUNKTION
           ELSE
               MOVE "ARCHIV" TO DI-FUNKTION
           END-IF
           PERFORM DIENST
           IF DI-ERFOLG
               MOVE KT-SATZ TO W-ORIG
               MOVE W-GEB-TEXT TO W-GEB-ORIG
               IF KT-ARCHIVIERT
                   MOVE "Archiviert. F11 würde jetzt endgültig löschen."
                     TO W-MELDUNG
               ELSE
                   MOVE "Wieder aktiv." TO W-MELDUNG
               END-IF
               PERFORM INFO-LADEN
           END-IF.

       DIENST.
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF NOT DI-ERFOLG
               MOVE DI-MELDUNG TO W-MELDUNG
           END-IF.

       INFO-LADEN.
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "KATEG" TO CD-LISTE
           MOVE KT-KATEGORIE TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-KAT-TEXT
           MOVE "ANREDE" TO CD-LISTE
           MOVE KP-ANREDE TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-ANREDE-TEXT
           CALL "BRIEFANR" USING KT-SATZ W-BRIEF-TEXT
           MOVE SPACES TO W-FIRMA-TEXT
           IF KT-FIRMA-NR NOT = SPACES
               MOVE KT-FIRMA-NR TO WK-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM WK-SATZ
               IF IO-OK AND WK-ART = "F"
                   MOVE WF-NAME TO W-FIRMA-TEXT
               ELSE
                   MOVE "(unbekannt - F4 sucht eine Firma)"
                     TO W-FIRMA-TEXT
               END-IF
           END-IF
           MOVE SPACES TO W-STDADR-TEXT W-KOMM-TEXT
           IF KT-NR = SPACES
               MOVE "Neue Person" TO W-TITEL
               MOVE SPACES TO W-KOPF-RECHTS
               MOVE "(nach dem Speichern: F9)" TO W-STDADR-TEXT
               MOVE "(nach dem Speichern: F10)" TO W-KOMM-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO W-TITEL
           STRING "Person " KT-NR DELIMITED BY SIZE INTO W-TITEL
           IF KT-ARCHIVIERT
               STRING "Person " KT-NR "  [ARCHIVIERT]"
                      DELIMITED BY SIZE INTO W-TITEL
           END-IF
           PERFORM KOPF-ZEITEN
           MOVE "STDADR" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF DI-ERFOLG
               PERFORM SATZ-LEEREN
               STRING FUNCTION TRIM(AD-STRASSE) " "
                      FUNCTION TRIM(AD-HAUSNR) "," DELIMITED BY SIZE
                 INTO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE AD-PLZ TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE AD-ORT TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE W-SATZ TO W-STDADR-TEXT
           ELSE
               IF KT-FIRMA-NR NOT = SPACES
                   MOVE "(keine eigene - Post geht an die Firma)"
                     TO W-STDADR-TEXT
               ELSE
                   MOVE "(keine - F9)" TO W-STDADR-TEXT
               END-IF
           END-IF
           PERFORM KOMM-TEXT-BILDEN.

       COPY kopfzeit-p.
       COPY bildkopf-p.
       COPY wort-p.
