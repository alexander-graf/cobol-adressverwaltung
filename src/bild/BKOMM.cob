       IDENTIFICATION DIVISION.
       PROGRAM-ID. BKOMM.
      * Kommunikationswege eines Kontakts: Liste, Bearbeiten, Neu,
      * Löschen (Aufbau wie BADRESS).
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
       01  W-TAB                   PIC X(132) OCCURS 99.
       01  W-I                     PIC 99.
       01  W-NR                    PIC 99.
       01  W-ZEILE                 PIC 99.
       01  W-ZL                    PIC Z9.
       01  W-ANZEIGE               PIC X(78).
       01  W-EINGABE               PIC XX.
       01  W-FERTIG                PIC X.
       01  W-MASKE-FERTIG          PIC X.
       01  W-ORIG                  PIC X(132).
       01  W-FEHLER                PIC X.
       01  W-TYP-TEXT              PIC X(30).
       01  W-LISTE                 PIC X(8).
       01  W-LISTE-TITEL           PIC X(50).
       01  W-WAHL                  PIC X(20).
       01  W-LISTEN-MELDUNG        PIC X(79).
      * Der Wert ist 100 Zeichen lang und wird auf zwei Zeilen gezeigt
       01  W-WERT.
           05  W-WERT1             PIC X(59).
           05  W-WERT2             PIC X(41).
       LINKAGE SECTION.
       01  BK-KT-NR                PIC X(7).
       01  BK-NAME                 PIC X(40).
       SCREEN SECTION.
       01  S-RAHMEN.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE
               "Zl  Typ          Bev  Wert" HIGHLIGHT.
       01  S-AUSWAHL.
           05  LINE 20 COLUMN 2 VALUE "Zeile bearbeiten:".
           05  LINE 20 COLUMN 20 PIC XX USING W-EINGABE
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
       01  S-KOMM.
           COPY bildkopf.
           05  LINE 3 COLUMN 2 VALUE "Typ .............:".
           05  LINE 3 COLUMN 21 PIC XX USING KO-TYP
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 3 COLUMN 25 PIC X(30) FROM W-TYP-TEXT.
           05  LINE 3 COLUMN 57 VALUE "F4 = Liste".
           05  LINE 5 COLUMN 2 VALUE "Nummer/Adresse ..:".
           05  LINE 5 COLUMN 21 PIC X(59) USING W-WERT1
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 6 COLUMN 21 PIC X(41) USING W-WERT2
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 2 VALUE "Bezeichnung .....:".
           05  LINE 8 COLUMN 21 PIC X(20) USING KO-BEZ
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 8 COLUMN 43 VALUE "z. B. Zentrale, privat".
           05  LINE 9 COLUMN 2 VALUE "Bevorzugt .......:".
           05  LINE 9 COLUMN 21 PIC X USING KO-BEVORZUGT
               HIGHLIGHT UNDERLINE PROMPT CHARACTER IS " ".
           05  LINE 9 COLUMN 24 VALUE "J/N - je Typ einer".
       PROCEDURE DIVISION USING BK-KT-NR BK-NAME.
           MOVE SPACES TO W-MELDUNG W-LISTEN-MELDUNG
           MOVE "N" TO W-FERTIG
           PERFORM UNTIL W-FERTIG = "J"
               PERFORM LISTE-LADEN
               PERFORM LISTE-ZEIGEN
           END-PERFORM
           GOBACK.

       LISTE-LADEN.
           MOVE 0 TO W-ANZ
           MOVE BK-KT-NR TO KO-KT-NR
           MOVE 0 TO KO-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM KO-SATZ
           PERFORM UNTIL NOT IO-OK OR W-ANZ >= 15
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KOIO" USING IO-PARM KO-SATZ
               IF IO-OK AND KO-KT-NR = BK-KT-NR
                   ADD 1 TO W-ANZ
                   MOVE KO-SATZ TO W-TAB(W-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM.

       LISTE-ZEIGEN.
           MOVE SPACES TO W-TITEL W-KOPF-RECHTS
           STRING "Kommunikation " BK-KT-NR " " FUNCTION TRIM(BK-NAME)
                  DELIMITED BY SIZE INTO W-TITEL
           MOVE "Enter bearbeiten  F5 neuer Eintrag  F3 zurück"
             TO W-TASTEN1
           MOVE "F1 Hilfe" TO W-TASTEN2
           PERFORM KOPF-BAUEN
           MOVE W-LISTEN-MELDUNG TO W-MELDUNG
           MOVE SPACES TO W-LISTEN-MELDUNG
           IF W-ANZ = 0
               MOVE "Noch kein Eintrag. F5 legt einen an." TO W-MELDUNG
           END-IF
           DISPLAY S-RAHMEN
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "KOMMTYP" TO CD-LISTE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ANZ
               MOVE W-TAB(W-I) TO KO-SATZ
               MOVE KO-TYP TO CD-CODE
               CALL "CODES" USING CD-PARM
               MOVE W-I TO W-ZL
               MOVE SPACES TO W-ANZEIGE
               MOVE W-ZL TO W-ANZEIGE(1:2)
               MOVE CD-TEXT TO W-ANZEIGE(5:12)
               MOVE KO-BEVORZUGT TO W-ANZEIGE(19:1)
               MOVE KO-WERT TO W-ANZEIGE(23:)
               COMPUTE W-ZEILE = W-I + 3
               DISPLAY W-ANZEIGE LINE W-ZEILE COLUMN 2
           END-PERFORM
           MOVE SPACES TO W-EINGABE
           ACCEPT S-AUSWAHL
           MOVE SPACES TO W-MELDUNG
           EVALUATE TRUE
               WHEN T-F3
                   MOVE "J" TO W-FERTIG
               WHEN T-F1
                   CALL "BHILFE"
               WHEN T-F5
                   INITIALIZE KO-SATZ
                   MOVE BK-KT-NR TO KO-KT-NR
                   MOVE "EM" TO KO-TYP
                   MOVE "N" TO KO-BEVORZUGT
                   PERFORM MASKE
               WHEN T-ENTER
                   MOVE 0 TO W-NR
                   IF W-EINGABE NOT = SPACES
                      AND FUNCTION TEST-NUMVAL(W-EINGABE) = 0
                       COMPUTE W-NR = FUNCTION NUMVAL(W-EINGABE)
                   END-IF
                   IF W-NR >= 1 AND W-NR <= W-ANZ
                       MOVE W-TAB(W-NR) TO KO-SATZ
                       PERFORM MASKE
                   ELSE
                       IF W-EINGABE NOT = SPACES
                           MOVE "Bitte eine Zeile der Liste."
                             TO W-LISTEN-MELDUNG
                       END-IF
                   END-IF
           END-EVALUATE.

      * Einen Eintrag bearbeiten. KO-LFD = 0 heißt: neu.
       MASKE.
           MOVE KO-SATZ TO W-ORIG
           MOVE KO-WERT TO W-WERT
           MOVE SPACES TO W-BESTAETIGEN W-MELDUNG W-TITEL
           STRING "Kommunikation " BK-KT-NR " " FUNCTION TRIM(BK-NAME)
                  DELIMITED BY SIZE INTO W-TITEL
           MOVE "F2 Speichern  F3 zurück  F4 Liste  F12 Löschen"
             TO W-TASTEN1
           MOVE "F1 Hilfe" TO W-TASTEN2
           PERFORM KOPF-BAUEN
           MOVE "N" TO W-MASKE-FERTIG
           PERFORM TEXTE-LADEN
           MOVE 0321 TO W-CURSOR
           PERFORM UNTIL W-MASKE-FERTIG = "J"
               DISPLAY S-KOMM
               ACCEPT S-KOMM
               MOVE W-WERT TO KO-WERT
               MOVE SPACES TO W-MELDUNG
               IF NOT (T-F3 AND W-BESTAETIGEN = "VERWERF")
                  AND NOT (T-F12 AND W-BESTAETIGEN = "LOESCHEN")
                   MOVE SPACES TO W-BESTAETIGEN
               END-IF
               EVALUATE TRUE
                   WHEN T-F1
                       CALL "BHILFE"
                   WHEN T-F2
                       PERFORM SPEICHERN
                   WHEN T-F3
                       IF KO-SATZ = W-ORIG
                          OR W-BESTAETIGEN = "VERWERF"
                           MOVE "J" TO W-MASKE-FERTIG
                       ELSE
                           MOVE "Nicht gespeichert! F3 erneut = "
                              & "verwerfen, F2 = speichern."
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
           MOVE "KOMMTYP" TO CD-LISTE
           MOVE KO-TYP TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE CD-TEXT TO W-TYP-TEXT.

       PRUEFEN.
           MOVE "N" TO W-FEHLER
           MOVE FUNCTION UPPER-CASE(KO-TYP) TO KO-TYP
           MOVE FUNCTION UPPER-CASE(KO-BEVORZUGT) TO KO-BEVORZUGT
           MOVE FUNCTION TRIM(KO-WERT) TO KO-WERT
           MOVE KO-WERT TO W-WERT
           PERFORM TEXTE-LADEN
           EVALUATE TRUE
               WHEN W-TYP-TEXT = SPACES
                   MOVE "Typ unbekannt (F4 zeigt die Liste)."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0321 TO W-CURSOR
               WHEN KO-BEVORZUGT NOT = "J" AND KO-BEVORZUGT NOT = "N"
                   MOVE "Bevorzugt: J oder N." TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0921 TO W-CURSOR
               WHEN KO-WERT = SPACES
                   MOVE "Bitte Nummer oder Adresse eingeben."
                     TO W-MELDUNG
                   MOVE "J" TO W-FEHLER
                   MOVE 0521 TO W-CURSOR
               WHEN KO-TYP = "EM"
                   MOVE "EMAIL" TO PR-FUNKTION
                   MOVE KO-WERT TO PR-WERT
                   CALL "PRUEF" USING PR-PARM
                   IF NOT PR-GUELTIG
                       MOVE PR-MELDUNG TO W-MELDUNG
                       MOVE "J" TO W-FEHLER
                       MOVE 0521 TO W-CURSOR
                   END-IF
           END-EVALUATE.

       SPEICHERN.
           PERFORM PRUEFEN
           IF W-FEHLER = "J"
               EXIT PARAGRAPH
           END-IF
           IF KO-LFD = 0
               MOVE "KOM-NEU" TO DI-FUNKTION
           ELSE
               MOVE "KOM-SPEI" TO DI-FUNKTION
           END-IF
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF DI-ERFOLG
               MOVE "Eintrag gespeichert." TO W-LISTEN-MELDUNG
               MOVE "J" TO W-MASKE-FERTIG
           ELSE
               MOVE DI-MELDUNG TO W-MELDUNG
           END-IF.

       LOESCHEN.
           IF KO-LFD = 0
               MOVE "J" TO W-MASKE-FERTIG
               EXIT PARAGRAPH
           END-IF
           IF W-BESTAETIGEN NOT = "LOESCHEN"
               MOVE "Diesen Eintrag löschen? F12 erneut = ja"
                 TO W-MELDUNG
               MOVE "LOESCHEN" TO W-BESTAETIGEN
               EXIT PARAGRAPH
           END-IF
           MOVE W-ORIG TO KO-SATZ
           MOVE "KOM-LOE" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           MOVE "Eintrag gelöscht." TO W-LISTEN-MELDUNG
           MOVE "J" TO W-MASKE-FERTIG.

       AUSWAHLLISTE.
           IF W-CUR-ZEILE = 3
               MOVE "KOMMTYP" TO W-LISTE
               MOVE "Typ wählen" TO W-LISTE-TITEL
               CALL "BLISTEW" USING W-LISTE W-LISTE-TITEL W-WAHL
               IF W-WAHL NOT = SPACES
                   MOVE W-WAHL TO KO-TYP
               END-IF
           ELSE
               MOVE "F4 gibt es beim Typ." TO W-MELDUNG
           END-IF
           PERFORM KOPF-BAUEN
           PERFORM TEXTE-LADEN.

       COPY bildkopf-p.
