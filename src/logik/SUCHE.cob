       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUCHE.
      * Sucht Kontakte nach den Kriterien in SU-PARM und füllt die
      * Trefferliste (höchstens 500, sortiert nach Suchschlüssel).
      *   SU-FIRMA-NR gesetzt: alle Ansprechpartner dieser Firma
      *   SU-BEGRIFF "abc":    Suchschlüssel beginnt mit ABC
      *   SU-BEGRIFF "*abc":   Suchschlüssel enthält ABC
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY wort-d.
       01  W-TEXT                  PIC X(100).
       01  W-BEGRIFF               PIC X(40).
       01  W-BEGRIFF-LEN           PIC 99.
       01  W-ORT                   PIC X(40).
       01  W-ORT-LEN               PIC 99.
       01  W-ORT-NORM              PIC X(40).
       01  W-PLZ-LEN               PIC 99.
       01  W-SW                    PIC X(40).
       01  W-SW-SATZ               PIC X(40).
       01  W-ENTHAELT              PIC X.
       01  W-ANZ                   PIC 99.
       01  W-I                     PIC 9.
       01  W-PASST                 PIC X.
       01  W-ADR-GELADEN           PIC X.
       LINKAGE SECTION.
       COPY suche.
       PROCEDURE DIVISION USING SU-PARM.
           MOVE 0 TO SU-ANZAHL
           MOVE "N" TO SU-VOLL
           PERFORM KRITERIEN-AUFBEREITEN
           EVALUATE TRUE
               WHEN SU-FIRMA-NR NOT = SPACES
                   MOVE SU-FIRMA-NR TO KT-FIRMA-NR
                   MOVE "START-FI" TO IO-FUNKTION
               WHEN W-ENTHAELT = "N" AND W-BEGRIFF NOT = SPACES
                   MOVE W-BEGRIFF TO KT-SUCH
                   MOVE "START-SU" TO IO-FUNKTION
               WHEN OTHER
                   MOVE LOW-VALUES TO KT-SUCH
                   MOVE "START-SU" TO IO-FUNKTION
           END-EVALUATE
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM UNTIL NOT IO-OK
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK
                   PERFORM SATZ-PRUEFEN
               END-IF
           END-PERFORM
           GOBACK.

       KRITERIEN-AUFBEREITEN.
           MOVE "N" TO W-ENTHAELT
           MOVE SPACES TO W-TEXT
           IF SU-BEGRIFF(1:1) = "*"
               MOVE "J" TO W-ENTHAELT
               MOVE SU-BEGRIFF(2:) TO W-TEXT
           ELSE
               MOVE SU-BEGRIFF TO W-TEXT
           END-IF
           CALL "SUCHKEY" USING W-TEXT W-BEGRIFF
           MOVE 0 TO W-BEGRIFF-LEN
           IF W-BEGRIFF NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(W-BEGRIFF))
                 TO W-BEGRIFF-LEN
           END-IF
           MOVE SU-ORT TO W-TEXT
           CALL "SUCHKEY" USING W-TEXT W-ORT
           MOVE 0 TO W-ORT-LEN
           IF W-ORT NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(W-ORT)) TO W-ORT-LEN
           END-IF
           MOVE 0 TO W-PLZ-LEN
           IF SU-PLZ NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(SU-PLZ))
                 TO W-PLZ-LEN
           END-IF
           MOVE SU-SCHLAGWORT TO W-TEXT
           CALL "SUCHKEY" USING W-TEXT W-SW.

      * Prüft den gelesenen Kontakt gegen alle Kriterien; beendet die
      * Schleife, sobald eine Präfixsuche über ihren Bereich hinaus ist.
       SATZ-PRUEFEN.
           MOVE "J" TO W-PASST
           MOVE "N" TO W-ADR-GELADEN
           IF SU-FIRMA-NR NOT = SPACES
               IF KT-FIRMA-NR NOT = SU-FIRMA-NR
                   MOVE "10" TO IO-STATUS
                   EXIT PARAGRAPH
               END-IF
           ELSE
               IF W-BEGRIFF-LEN > 0
                   IF W-ENTHAELT = "N"
                       IF KT-SUCH(1:W-BEGRIFF-LEN)
                          NOT = W-BEGRIFF(1:W-BEGRIFF-LEN)
                           MOVE "10" TO IO-STATUS
                           EXIT PARAGRAPH
                       END-IF
                   ELSE
                       MOVE 0 TO W-ANZ
                       INSPECT KT-SUCH TALLYING W-ANZ
                           FOR ALL W-BEGRIFF(1:W-BEGRIFF-LEN)
                       IF W-ANZ = 0
                           MOVE "N" TO W-PASST
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF SU-ARCHIV = "J"
               IF NOT KT-ARCHIVIERT
                   MOVE "N" TO W-PASST
               END-IF
           ELSE
               IF KT-ARCHIVIERT
                   MOVE "N" TO W-PASST
               END-IF
           END-IF
           IF SU-ART NOT = SPACES AND SU-ART NOT = KT-ART
               MOVE "N" TO W-PASST
           END-IF
           IF SU-KATEGORIE NOT = SPACES
              AND SU-KATEGORIE NOT = KT-KATEGORIE
               MOVE "N" TO W-PASST
           END-IF
           IF W-PASST = "J" AND W-SW NOT = SPACES
               PERFORM SCHLAGWORT-PRUEFEN
           END-IF
           IF W-PASST = "J" AND (W-PLZ-LEN > 0 OR W-ORT-LEN > 0)
               PERFORM ADRESSE-HOLEN
               IF W-PLZ-LEN > 0
                   IF AD-PLZ(1:W-PLZ-LEN) NOT = SU-PLZ(1:W-PLZ-LEN)
                       MOVE "N" TO W-PASST
                   END-IF
               END-IF
               IF W-ORT-LEN > 0
                   MOVE AD-ORT TO W-TEXT
                   CALL "SUCHKEY" USING W-TEXT W-ORT-NORM
                   IF W-ORT-NORM(1:W-ORT-LEN) NOT = W-ORT(1:W-ORT-LEN)
                       MOVE "N" TO W-PASST
                   END-IF
               END-IF
           END-IF
           IF W-PASST = "J"
               IF SU-ANZAHL >= 500
                   MOVE "J" TO SU-VOLL
                   MOVE "10" TO IO-STATUS
               ELSE
                   PERFORM TREFFER-EINTRAGEN
               END-IF
           END-IF.

       SCHLAGWORT-PRUEFEN.
           MOVE "N" TO W-PASST
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 5
               IF KT-SCHLAGWORT(W-I) NOT = SPACES
                   MOVE KT-SCHLAGWORT(W-I) TO W-TEXT
                   CALL "SUCHKEY" USING W-TEXT W-SW-SATZ
                   IF W-SW-SATZ = W-SW
                       MOVE "J" TO W-PASST
                   END-IF
               END-IF
           END-PERFORM.

       ADRESSE-HOLEN.
           IF W-ADR-GELADEN = "N"
               MOVE "STDADR" TO DI-FUNKTION
               CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
               IF NOT DI-ERFOLG
                   MOVE SPACES TO AD-SATZ
               END-IF
               MOVE "J" TO W-ADR-GELADEN
           END-IF.

       TREFFER-EINTRAGEN.
           PERFORM ADRESSE-HOLEN
           ADD 1 TO SU-ANZAHL
           MOVE KT-NR TO ST-NR(SU-ANZAHL)
           MOVE KT-ART TO ST-ART(SU-ANZAHL)
           MOVE KT-KATEGORIE TO ST-KAT(SU-ANZAHL)
           MOVE AD-PLZ TO ST-PLZ(SU-ANZAHL)
           MOVE AD-ORT TO ST-ORT(SU-ANZAHL)
           PERFORM SATZ-LEEREN
           IF KT-IST-FIRMA
               MOVE KF-NAME TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE KF-RECHTSFORM TO W-WORT
               PERFORM WORT-ANHAENGEN
           ELSE
               STRING FUNCTION TRIM(KP-NACHNAME) "," DELIMITED BY SIZE
                 INTO W-WORT
               IF KP-VORNAME = SPACES
                   MOVE KP-NACHNAME TO W-WORT
               END-IF
               PERFORM WORT-ANHAENGEN
               MOVE KP-VORNAME TO W-WORT
               PERFORM WORT-ANHAENGEN
           END-IF
           MOVE W-SATZ TO ST-NAME(SU-ANZAHL).

       COPY wort-p.
