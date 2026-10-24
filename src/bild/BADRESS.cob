       IDENTIFICATION DIVISION.
       PROGRAM-ID. BADRESS.
      * Adressen eines Kontakts: Liste, Bearbeiten, Neu, Löschen.
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
       01  W-ANZ                   PIC 99.
       01  W-TAB                   PIC X(243) OCCURS 99.
       01  W-I                     PIC 99.
       01  W-NR                    PIC 99.
       01  W-ZEILE                 PIC 99.
       01  W-ZL                    PIC Z9.
       01  W-ANZEIGE               PIC X(78).
       01  W-EINGABE               PIC XX.
       01  W-FERTIG                PIC X.
       01  W-MASKE-FERTIG          PIC X.
       01  W-ORIG                  PIC X(243).
       01  W-FEHLER                PIC X.
       01  W-TYP-TEXT              PIC X(30).
       01  W-LAND-TEXT             PIC X(30).
       01  W-LISTE                 PIC X(8).
       01  W-LISTE-TITEL           PIC X(50).
       01  W-WAHL                  PIC X(20).
       01  W-LISTEN-MELDUNG        PIC X(79).
       LINKAGE SECTION.
       01  BA-KT-NR                PIC X(7).
       01  BA-NAME                 PIC X(40).
       SCREEN SECTION.
       01  S-RAHMEN.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE
               "Zl  Typ Std  Anschrift" HIGHLIGHT.
       01  S-AUSWAHL.
           05  LINE 20 COLUMN 2 VALUE "Zeile/Befehl ...:".
           05  LINE 20 COLUMN 20 PIC XX USING W-EINGABE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
       01  S-ADRESSE.
           COPY bildkopf.
           COPY befehl.
           05  LINE 3 COLUMN 2 VALUE "Adresstyp .......:".
           05  LINE 3 COLUMN 21 PIC X USING AD-TYP
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 3 COLUMN 24 PIC X(30) FROM W-TYP-TEXT.
           05  LINE 3 COLUMN 57 VALUE "? = Liste".
           05  LINE 4 COLUMN 2 VALUE "Standardadresse .:".
           05  LINE 4 COLUMN 21 PIC X USING AD-STANDARD
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 4 COLUMN 24 VALUE "J/N - für Post und Export".
           05  LINE 6 COLUMN 2 VALUE "Straße/Postfach .:".
           05  LINE 6 COLUMN 21 PIC X(50) USING AD-STRASSE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 7 COLUMN 2 VALUE "Hausnummer ......:".
           05  LINE 7 COLUMN 21 PIC X(10) USING AD-HAUSNR
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 2 VALUE "Adresszusatz ....:".
           05  LINE 8 COLUMN 21 PIC X(50) USING AD-ZUSATZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 2 VALUE "PLZ .............:".
           05  LINE 9 COLUMN 21 PIC X(10) USING AD-PLZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 10 COLUMN 2 VALUE "Ort .............:".
           05  LINE 10 COLUMN 21 PIC X(40) USING AD-ORT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 11 COLUMN 2 VALUE "Ortsteil ........:".
           05  LINE 11 COLUMN 21 PIC X(40) USING AD-ORTSTEIL
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 12 COLUMN 2 VALUE "Bundesland/Region:".
           05  LINE 12 COLUMN 21 PIC X(30) USING AD-REGION
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 13 COLUMN 2 VALUE "Land (ISO-Code) .:".
           05  LINE 13 COLUMN 21 PIC XX USING AD-LAND
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 13 COLUMN 24 PIC X(30) FROM W-LAND-TEXT.
           05  LINE 13 COLUMN 57 VALUE "? = Liste".
       PROCEDURE DIVISION USING BA-KT-NR BA-NAME.
           MOVE SPACES TO W-MELDUNG W-LISTEN-MELDUNG
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               PERFORM LISTE-LADEN
               PERFORM LISTE-ZEIGEN
           END-PERFORM
           GOBACK.

       LISTE-LADEN.
           MOVE 0 TO W-ANZ
           MOVE BA-KT-NR TO AD-KT-NR
           MOVE 0 TO AD-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM AD-SATZ
           PERFORM UNTIL NOT IO-OK OR W-ANZ >= 15
               MOVE "WEITER" TO IO-FUNKTION
               CALL "ADIO" USING IO-PARM AD-SATZ
               IF IO-OK AND AD-KT-NR = BA-KT-NR
                   ADD 1 TO W-ANZ
                   MOVE AD-SATZ TO W-TAB(W-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM.

       LISTE-ZEIGEN.
           MOVE SPACES TO W-TITEL W-KOPF-RECHTS
           STRING "Adressen " BA-KT-NR " " FUNCTION TRIM(BA-NAME)
                  DELIMITED BY SIZE INTO W-TITEL
           MOVE "Zeilennummer = bearbeiten  N=Neue Adresse  Z=Zurück  "
              & "?=Hilfe" TO W-TASTEN1
           MOVE SPACES TO W-TASTEN2
           MOVE "?   1001Z   1003N   1005" TO W-BEF-TAB
           MOVE "J" TO W-BEF-LISTE
           PERFORM KOPF-BAUEN
           MOVE W-LISTEN-MELDUNG TO W-MELDUNG
           MOVE SPACES TO W-LISTEN-MELDUNG
           IF W-ANZ = 0
               MOVE "Noch keine Adresse. N legt eine an." TO W-MELDUNG
           END-IF
           DISPLAY S-RAHMEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ANZ
               MOVE W-TAB(W-I) TO AD-SATZ
               MOVE W-I TO W-ZL
               MOVE SPACES TO W-ANZEIGE
               STRING W-ZL "   " AD-TYP "   " AD-STANDARD "    "
                      FUNCTION TRIM(AD-STRASSE) " "
                      FUNCTION TRIM(AD-HAUSNR) ", "
                      FUNCTION TRIM(AD-PLZ) " "
                      FUNCTION TRIM(AD-ORT) "  " AD-LAND
                      DELIMITED BY SIZE INTO W-ANZEIGE
               COMPUTE W-ZEILE = W-I + 3
               DISPLAY W-ANZEIGE LINE W-ZEILE COLUMN 2
           END-PERFORM
           MOVE SPACES TO W-EINGABE
           ACCEPT S-AUSWAHL
           MOVE SPACES TO W-MELDUNG
           MOVE W-EINGABE TO W-BEFEHL
           PERFORM BEFEHL-AUSWERTEN
           EVALUATE TRUE
               WHEN T-F3
                   MOVE "J" TO W-FERTIG
               WHEN T-F1
                   CALL "BHILFE"
               WHEN T-F5
                   INITIALIZE AD-SATZ
                   MOVE BA-KT-NR TO AD-KT-NR
                   MOVE "H" TO AD-TYP
                   IF BA-KT-NR(1:1) = "P"
                       MOVE "W" TO AD-TYP
                   END-IF
                   MOVE "N" TO AD-STANDARD
                   MOVE "DE" TO AD-LAND
                   PERFORM MASKE
               WHEN T-ENTER
                   MOVE 0 TO W-NR
                   IF W-EINGABE NOT = SPACES
                      AND FUNCTION TEST-NUMVAL(W-EINGABE) = 0
                       COMPUTE W-NR = FUNCTION NUMVAL(W-EINGABE)
                   END-IF
                   IF W-NR >= 1 AND W-NR <= W-ANZ
                       MOVE W-TAB(W-NR) TO AD-SATZ
                       PERFORM MASKE
                   ELSE
                       IF W-EINGABE NOT = SPACES
                           MOVE "Bitte eine Zeile der Liste."
                             TO W-LISTEN-MELDUNG
                       END-IF
                   END-IF
           END-EVALUATE.

      * Eine Adresse bearbeiten. AD-LFD = 0 heißt: neue Adresse.
       MASKE.
           MOVE AD-SATZ TO W-ORIG
           MOVE SPACES TO W-BESTAETIGEN W-MELDUNG W-TITEL
           STRING "Adresse " BA-KT-NR " " FUNCTION TRIM(BA-NAME)
                  DELIMITED BY SIZE INTO W-TITEL
           MOVE "S=Speichern  Z=Zurück  L=Löschen  ?=Hilfe" TO W-TASTEN1
           MOVE "? im Feld = Liste" TO W-TASTEN2
           MOVE "?   1001S   1002Z   1003L   1012" TO W-BEF-TAB
           MOVE "N" TO W-BEF-LISTE
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-MASKE-FERTIG
           PERFORM TEXTE-LADEN
           MOVE 0321 TO W-CURSOR
           PERFORM UNTIL W-MASKE-FERTIG = "J"
               DISPLAY S-ADRESSE
               ACCEPT S-ADRESSE
               MOVE SPACES TO W-MELDUNG
               IF NOT (T-F3 AND W-BESTAETIGEN = "VERWERF")
                  AND NOT (T-F12 AND W-BESTAETIGEN = "LOESCHEN")
                   MOVE SPACES TO W-BESTAETIGEN
               END-IF
               PERFORM BEFEHL-AUSWERTEN
               PERFORM FRAGE-FELDER
               EVALUATE TRUE
                   WHEN T-UNBEKANNT
                       CONTINUE
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-F2
                       PERFORM SPEICHERN
                   WHEN T-F3
                       IF AD-SATZ = W-ORIG
                          OR W-BESTAETIGEN = "VERWERF"
                           MOVE "J" TO W-MASKE-FERTIG
                       ELSE
                           MOVE "Nicht gespeichert! Z erneut = "
                              & "verwerfen, S = speichern."
                             TO W-MELDUNG
                           MOVE "VERWERF" TO W-BESTAETIGEN
                       END-IF
                   WHEN T-F4
                       PERFORM AUSWAHLLISTE
                   WHEN T-F12
                       PERFORM LOESCHEN
                   WHEN OTHER
                       PERFORM PRUEFEN
               END-EVALUATE
           END-PERFORM.

       TEXTE-LADEN.
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "ADRTYP" TO CD-LISTE
           MOVE AD-TYP TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-TYP-TEXT
           MOVE "LAND" TO CD-LISTE
           MOVE AD-LAND TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-LAND-TEXT.

       PRUEFEN.
           MOVE "N" TO W-FEHLER
           MOVE FUNCTION UPPER-CASE(AD-TYP) TO AD-TYP
           MOVE FUNCTION UPPER-CASE(AD-STANDARD) TO AD-STANDARD
           MOVE FUNCTION UPPER-CASE(AD-LAND) TO AD-LAND
           PERFORM TEXTE-LADEN
           EVALUATE TRUE
               WHEN W-TYP-TEXT = SPACES
                   MOVE "Adresstyp unbekannt (? im Feld = Liste)."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0321 TO W-CURSOR
               WHEN AD-STANDARD NOT = "J" AND AD-STANDARD NOT = "N"
                   MOVE "Standardadresse: J oder N." TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0421 TO W-CURSOR
               WHEN AD-STRASSE = SPACES AND AD-ORT = SPACES
                   MOVE "Bitte mindestens Straße oder Ort angeben."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0621 TO W-CURSOR
           END-EVALUATE
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           MOVE "LAND" TO PR-FUNKTION
           MOVE AD-LAND TO PR-WERT
           CALL "PRUEF" USING PR-PARM
           IF NOT PR-GUELTIG
               MOVE PR-MELDUNG TO W-MELDUNG
               MOVE "J" TO W-FEHLER
               MOVE 1321 TO W-CURSOR
               EXIT PARAGRAPH
           END-IF
           MOVE "PLZ" TO PR-FUNKTION
           MOVE AD-PLZ TO PR-WERT
           MOVE AD-LAND TO PR-LAND
           CALL "PRUEF" USING PR-PARM
           IF NOT PR-GUELTIG
               MOVE PR-MELDUNG TO W-MELDUNG
               MOVE "J" TO W-FEHLER
               MOVE 0921 TO W-CURSOR
           END-IF.

       SPEICHERN.
           PERFORM PRUEFEN
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           IF AD-LFD = 0
               MOVE "ADR-NEU" TO DI-FUNKTION
           ELSE
               MOVE "ADR-SPEI" TO DI-FUNKTION
           END-IF
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF DI-ERFOLG
               MOVE "Adresse gespeichert." TO W-LISTEN-MELDUNG
               MOVE "J" TO W-MASKE-FERTIG
           ELSE
               MOVE DI-MELDUNG TO W-MELDUNG
           END-IF.

       LOESCHEN.
           IF AD-LFD = 0
               MOVE "J" TO W-MASKE-FERTIG
               EXIT PARAGRAPH
           END-IF
           IF W-BESTAETIGEN NOT = "LOESCHEN"
               MOVE "Diese Adresse löschen? L erneut = ja"
                 TO W-MELDUNG
               MOVE "LOESCHEN" TO W-BESTAETIGEN
               EXIT PARAGRAPH
           END-IF
           MOVE W-ORIG TO AD-SATZ
           MOVE "ADR-LOE" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           MOVE "Adresse gelöscht." TO W-LISTEN-MELDUNG
           MOVE "J" TO W-MASKE-FERTIG.

       AUSWAHLLISTE.
           EVALUATE W-CUR-ZEILE
               WHEN 3
                   MOVE "ADRTYP" TO W-LISTE
                   MOVE "Adresstyp wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO AD-TYP
                   END-IF
               WHEN 13
                   MOVE "LAND" TO W-LISTE
                   MOVE "Land wählen" TO W-LISTE-TITEL
                   CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
                   IF W-WAHL NOT = SPACES
                       MOVE W-WAHL TO AD-LAND
                   END-IF
               WHEN OTHER
                   MOVE "Liste gibt es bei Adresstyp und Land."
                     TO W-MELDUNG
           END-EVALUATE
           PERFORM KOPF-BAUEN
           PERFORM TEXTE-LADEN.

      * ? als erstes Zeichen eines Codefelds öffnet die Liste wie F4.
       FRAGE-FELDER.
           IF NOT T-ENTER
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN AD-TYP(1:1) = "?"
                   MOVE SPACES TO AD-TYP
                   MOVE 0321 TO W-CURSOR
                   MOVE 1004 TO W-TASTE
               WHEN AD-LAND(1:1) = "?"
                   MOVE SPACES TO AD-LAND
                   MOVE 1321 TO W-CURSOR
                   MOVE 1004 TO W-TASTE
           END-EVALUATE.

       COPY befehl-p.
       COPY bildkopf-p.
