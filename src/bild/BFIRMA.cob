       IDENTIFICATION DIVISION.
       PROGRAM-ID. BFIRMA.
      * Maske Firma: anlegen, ändern, archivieren, löschen; von hier
      * aus Adressen, Kommunikation, Anschrift und Ansprechpartner.
      * BF-NR leer = neue Firma. BF-RUECK "SUCH" = zurück zur Suche.
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
       COPY codes.
       COPY pruef.
       COPY suche.
       COPY bwahl.
       COPY austausch.
       COPY wort-d.
       01  W-ORIG                  PIC X(781).
       01  W-FERTIG                PIC X.
       01  W-FEHLER                PIC X.
       01  W-KAT-TEXT              PIC X(30).
       01  W-STDADR-TEXT           PIC X(60).
       01  W-KOMM-TEXT             PIC X(60).
       01  W-AP-TEXT               PIC X(60).
       01  W-KURZNAME              PIC X(40).
       01  W-LISTE                 PIC X(8).
       01  W-LISTE-TITEL           PIC X(50).
       01  W-WAHL                  PIC X(20).
       01  W-PERSON-NR             PIC X(7).
       01  W-PERSON-RUECK          PIC X(8).
       01  W-AP-FERTIG             PIC X.
       01  W-ANZ-A                 PIC ZZZ9.
       LINKAGE SECTION.
       01  BF-NR                   PIC X(7).
       01  BF-RUECK                PIC X(8).
       SCREEN SECTION.
       01  S-FIRMA.
           COPY bildkopf.
           COPY befehl.
           05  LINE 3 COLUMN 2 VALUE "Firmenname ......:".
           05  LINE 3 COLUMN 21 PIC X(60) USING KF-NAME
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 4 COLUMN 2 VALUE "Namenszusatz ....:".
           05  LINE 4 COLUMN 21 PIC X(60) USING KF-ZUSATZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 5 COLUMN 2 VALUE "Rechtsform ......:".
           05  LINE 5 COLUMN 21 PIC X(20) USING KF-RECHTSFORM
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 5 COLUMN 43 VALUE "? = Liste".
           05  LINE 6 COLUMN 2 VALUE "Branche .........:".
           05  LINE 6 COLUMN 21 PIC X(30) USING KF-BRANCHE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 7 COLUMN 2 VALUE "Website .........:".
           05  LINE 7 COLUMN 21 PIC X(60) USING KF-WEBSITE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 2 VALUE "USt-IdNr. .......:".
           05  LINE 8 COLUMN 21 PIC X(15) USING KF-USTID
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 38 VALUE "Steuernummer:".
           05  LINE 8 COLUMN 52 PIC X(20) USING KF-STEUERNR
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 2 VALUE "Registergericht .:".
           05  LINE 9 COLUMN 21 PIC X(30) USING KF-REGGERICHT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 52 VALUE "HR-Nr.:".
           05  LINE 9 COLUMN 60 PIC X(20) USING KF-HRNR
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 10 COLUMN 2 VALUE "Kundennr./Ref. ..:".
           05  LINE 10 COLUMN 21 PIC X(20) USING KF-REFERENZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 11 COLUMN 2 VALUE "Kategorie .......:".
           05  LINE 11 COLUMN 21 PIC XX USING KT-KATEGORIE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 11 COLUMN 25 PIC X(30) FROM W-KAT-TEXT.
           05  LINE 11 COLUMN 57 VALUE "? = Liste".
           05  LINE 12 COLUMN 2 VALUE "Schlagwörter ....:".
           05  LINE 12 COLUMN 21 PIC X(11) USING KT-SCHLAGWORT(1)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 33 PIC X(11) USING KT-SCHLAGWORT(2)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 45 PIC X(11) USING KT-SCHLAGWORT(3)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 57 PIC X(11) USING KT-SCHLAGWORT(4)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 69 PIC X(11) USING KT-SCHLAGWORT(5)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 13 COLUMN 2 VALUE "Notiz ...........:".
           05  LINE 13 COLUMN 21 PIC X(60) USING KT-NOTIZ(1)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 14 COLUMN 21 PIC X(60) USING KT-NOTIZ(2)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 15 COLUMN 21 PIC X(60) USING KT-NOTIZ(3)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 16 COLUMN 21 PIC X(60) USING KT-NOTIZ(4)
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 18 COLUMN 2 VALUE "Standardadresse .:".
           05  LINE 18 COLUMN 21 PIC X(60) FROM W-STDADR-TEXT.
           05  LINE 19 COLUMN 2 VALUE "Kommunikation ...:".
           05  LINE 19 COLUMN 21 PIC X(60) FROM W-KOMM-TEXT.
           05  LINE 20 COLUMN 2 VALUE "Ansprechpartner .:".
           05  LINE 20 COLUMN 21 PIC X(60) FROM W-AP-TEXT.
       PROCEDURE DIVISION USING BF-NR BF-RUECK.
           MOVE SPACES TO BF-RUECK W-MELDUNG W-BESTAETIGEN
           MOVE "S=Speichern  Z=Zurück  SU=Suche  AN=Anschrift  "
              & "AP=Ansprechpartner  ?=Hilfe" TO W-TASTEN1
           MOVE "A=Adressen  K=Kommunikation  AR=Archiv  L=Löschen  "
              & "? im Feld = Liste" TO W-TASTEN2
           MOVE "?   1001S   1002Z   1003SU  1006AN  1007AP  1008"
              & "A   1009K   1010L   1011AR  1012" TO W-BEF-TAB
           IF BF-NR = SPACES
               INITIALIZE KT-SATZ
               MOVE "F" TO KT-ART
               MOVE "N" TO KT-ARCHIV
               MOVE "IN" TO KT-KATEGORIE
           ELSE
               MOVE BF-NR TO KT-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF NOT IO-OK
                   GOBACK
               END-IF
           END-IF
           MOVE KT-SATZ TO W-ORIG
           PERFORM INFO-LADEN
           MOVE 0321 TO W-CURSOR
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               PERFORM KOPF-BAUEN
               DISPLAY S-FIRMA
               ACCEPT S-FIRMA
               MOVE SPACES TO W-MELDUNG
               PERFORM BEFEHL-AUSWERTEN
               PERFORM FRAGE-FELDER
               PERFORM TASTE-AUSWERTEN
           END-PERFORM
           GOBACK.

       TASTE-AUSWERTEN.
      *    Rückfragen gelten nur für den nächsten Tastendruck
           IF NOT (T-F2 AND W-BESTAETIGEN = "DUBL")
              AND NOT (T-F3 AND W-BESTAETIGEN = "VERWERF")
              AND NOT (T-F6 AND W-BESTAETIGEN = "VERWERF")
              AND NOT (T-F11 AND W-BESTAETIGEN = "TILGEN")
               MOVE SPACES TO W-BESTAETIGEN
           END-IF
           EVALUATE TRUE
               WHEN T-UNBEKANNT
                   CONTINUE
               WHEN T-F1
                   CALL "BHILFE"
               WHEN T-F2
                   PERFORM SPEICHERN
               WHEN T-F3
                   PERFORM VERLASSEN
               WHEN T-F6
                   PERFORM VERLASSEN
                   IF W-FERTIG = "J"
                       MOVE "SUCH" TO BF-RUECK
                   END-IF
               WHEN T-F4
                   PERFORM AUSWAHLLISTE
               WHEN T-ENTER
                   PERFORM PRUEFEN
                   IF W-FEHLER = "N"
                       MOVE "Eingaben in Ordnung. S speichert."
                         TO W-MELDUNG
                       MOVE 0214 TO W-CURSOR
                   END-IF
               WHEN KT-NR = SPACES
                   MOVE "Bitte erst speichern (S)." TO W-MELDUNG
               WHEN T-F7
                   CALL "BANSCHR" USING KT-NR
               WHEN T-F8
                   PERFORM ANSPRECHPARTNER
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
           MOVE FUNCTION UPPER-CASE(KF-USTID) TO KF-USTID
           IF KF-NAME = SPACES
               MOVE "J" TO W-FEHLER
               MOVE "Der Firmenname fehlt." TO W-MELDUNG
               MOVE 0321 TO W-CURSOR
               EXIT PARAGRAPH
           END-IF
           MOVE "PRUEF" TO CD-FUNKTION
           MOVE "KATEG" TO CD-LISTE
           MOVE KT-KATEGORIE TO CD-CODE
           CALL "CODES" USING CD-PARM
           IF NOT CD-GUELTIG
               MOVE "J" TO W-FEHLER
               MOVE "Kategorie unbekannt (? im Feld = Liste)."
                 TO W-MELDUNG
               MOVE 1121 TO W-CURSOR
               EXIT PARAGRAPH
           END-IF
           MOVE "USTID" TO PR-FUNKTION
           MOVE KF-USTID TO PR-WERT
           CALL "PRUEF" USING PR-PARM
           IF NOT PR-GUELTIG
               MOVE "J" TO W-FEHLER
               MOVE PR-MELDUNG TO W-MELDUNG
               MOVE 0821 TO W-CURSOR
           END-IF
           PERFORM INFO-LADEN.

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
                              " - S erneut speichert trotzdem."
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
               STRING "Gespeichert als " KT-NR "."
                      DELIMITED BY SIZE INTO W-MELDUNG
               PERFORM INFO-LADEN
           END-IF.

      * Ungespeicherte Änderungen: erst nachfragen
       VERLASSEN.
           IF KT-SATZ = W-ORIG OR W-BESTAETIGEN = "VERWERF"
               MOVE "J" TO W-FERTIG
           ELSE
               MOVE "Nicht gespeichert! Nochmal = verwerfen, "
                  & "S = speichern." TO W-MELDUNG
               MOVE "VERWERF" TO W-BESTAETIGEN
           END-IF.

       AUSWAHLLISTE.
           EVALUATE W-CUR-ZEILE
               WHEN 5
                   MOVE "RECHTSF" TO W-LISTE
                   MOVE "Rechtsform wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO KF-RECHTSFORM
                   END-IF
               WHEN 11
                   MOVE "KATEG" TO W-LISTE
                   MOVE "Kategorie wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO KT-KATEGORIE
                       PERFORM INFO-LADEN
                   END-IF
               WHEN OTHER
                   MOVE "Liste gibt es bei Rechtsform und Kategorie."
                     TO W-MELDUNG
           END-EVALUATE.

      * ? als erstes Zeichen eines Codefelds öffnet die Liste wie F4.
       FRAGE-FELDER.
           IF NOT T-ENTER
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN KF-RECHTSFORM(1:1) = "?"
                   MOVE SPACES TO KF-RECHTSFORM
                   MOVE 0521 TO W-CURSOR
                   MOVE 1004 TO W-TASTE
               WHEN KT-KATEGORIE(1:1) = "?"
                   MOVE SPACES TO KT-KATEGORIE
                   MOVE 1121 TO W-CURSOR
                   MOVE 1004 TO W-TASTE
           END-EVALUATE.

      * Liste der Ansprechpartner; N legt eine neue Person an, die
      * gleich dieser Firma zugeordnet ist.
       ANSPRECHPARTNER.
           INITIALIZE SU-PARM
           MOVE KT-NR TO SU-FIRMA-NR
           PERFORM KURZNAME-BILDEN
           STRING "Ansprechpartner " FUNCTION TRIM(W-KURZNAME)
                  DELIMITED BY SIZE INTO BW-TITEL
           MOVE "S" TO BW-MODUS
           MOVE SPACES TO BW-MELDUNG
           MOVE "N" TO W-AP-FERTIG
           PERFORM UNTIL W-AP-FERTIG = "J"
               CALL "BWAHL" USING SU-PARM BW-PARM
               MOVE SPACES TO W-PERSON-RUECK
               EVALUATE BW-RUECK
                   WHEN "WAHL"
                       MOVE BW-NR TO W-PERSON-NR
                       CALL "BPERSON" USING W-PERSON-NR W-PERSON-RUECK
                                            KT-NR
                   WHEN "NEU"
                       MOVE SPACES TO W-PERSON-NR
                       CALL "BPERSON" USING W-PERSON-NR W-PERSON-RUECK
                                            KT-NR
                   WHEN "ETIK"
                       INITIALIZE AU-PARM
                       MOVE "~/adrv-etiketten.txt" TO AU-DATEI
                       CALL "ETIKETT" USING SU-PARM AU-PARM
                       STRING FUNCTION TRIM(AU-MELDUNG) " nach "
                              FUNCTION TRIM(AU-DATEI)
                              DELIMITED BY SIZE INTO BW-MELDUNG
                   WHEN OTHER
                       MOVE "J" TO W-AP-FERTIG
               END-EVALUATE
               IF W-PERSON-RUECK = "SUCH"
                   MOVE "J" TO W-AP-FERTIG
               END-IF
           END-PERFORM
           PERFORM INFO-LADEN.

       TILGEN.
           IF NOT KT-ARCHIVIERT
               MOVE "Endgültig löschen geht nur bei archivierten "
                  & "Kontakten (erst AR)." TO W-MELDUNG
               EXIT PARAGRAPH
           END-IF
           IF W-BESTAETIGEN NOT = "TILGEN"
               MOVE "Firma samt Adressen und Kommunikation "
                  & "endgültig löschen? L erneut = ja" TO W-MELDUNG
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
               IF KT-ARCHIVIERT
                   MOVE "Archiviert. L würde jetzt endgültig löschen."
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

      * Anzeigefelder: Kategorietext, Standardadresse, bevorzugte
      * Kommunikation, Zahl der Ansprechpartner, Kopfzeile.
       INFO-LADEN.
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "KATEG" TO CD-LISTE
           MOVE KT-KATEGORIE TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-KAT-TEXT
           MOVE SPACES TO W-STDADR-TEXT W-KOMM-TEXT W-AP-TEXT
           IF KT-NR = SPACES
               MOVE "Neue Firma" TO W-TITEL
               MOVE SPACES TO W-KOPF-RECHTS
               MOVE "(nach dem Speichern: A)" TO W-STDADR-TEXT
               MOVE "(nach dem Speichern: K)" TO W-KOMM-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO W-TITEL
           STRING "Firma " KT-NR DELIMITED BY SIZE INTO W-TITEL
           IF KT-ARCHIVIERT
               STRING "Firma " KT-NR "  [ARCHIVIERT]"
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
               MOVE "(keine - A)" TO W-STDADR-TEXT
           END-IF
           PERFORM KOMM-TEXT-BILDEN
           INITIALIZE SU-PARM
           MOVE KT-NR TO SU-FIRMA-NR
           CALL "SUCHE" USING SU-PARM
           MOVE SU-ANZAHL TO W-ANZ-A
           STRING FUNCTION TRIM(W-ANZ-A) " (AP)" DELIMITED BY SIZE
             INTO W-AP-TEXT.

       COPY kopfzeit-p.
       COPY befehl-p.
       COPY bildkopf-p.
       COPY wort-p.
