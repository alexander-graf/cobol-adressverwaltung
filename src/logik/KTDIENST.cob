       IDENTIFICATION DIVISION.
       PROGRAM-ID. KTDIENST.
      * Geschäftsregeln rund um Kontakte, Adressen und Kommunikation.
      * Die Masken sprechen Dateien nur lesend direkt an; alles, was
      * schreibt, geht hier durch. Funktionen siehe dienst.cpy.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY anschr.
       COPY kontakt REPLACING LEADING ==KT-== BY ==WK-==
                              LEADING ==KF-== BY ==WF-==
                              LEADING ==KP-== BY ==WP-==.
       COPY adresse REPLACING LEADING ==AD-== BY ==WA-==.
       COPY komm REPLACING LEADING ==KO-== BY ==WO-==.
       01  W-JETZT                 PIC 9(14).
       01  W-NR                    PIC X(7).
       01  W-NV-STATUS             PIC XX.
       01  W-SUCH-TEXT             PIC X(100).
       01  W-I                     PIC 9(4).
       01  W-MAX-LFD               PIC 99.
       01  W-GEFUNDEN              PIC X.
       01  W-TYP                   PIC XX.
       01  W-ADR-ANZ               PIC 99.
       01  W-ADR                   PIC X(243) OCCURS 99.
       01  W-KOM-ANZ               PIC 99.
       01  W-KOM                   PIC X(132) OCCURS 99.
       01  W-NR-ANZ                PIC 9(4).
       01  W-NR-E                  PIC X(7) OCCURS 2000.
       LINKAGE SECTION.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       PROCEDURE DIVISION USING DI-PARM KT-SATZ AD-SATZ KO-SATZ.
           MOVE "J" TO DI-OK
           MOVE SPACES TO DI-MELDUNG
           MOVE FUNCTION CURRENT-DATE(1:14) TO W-JETZT
           EVALUATE DI-FUNKTION
               WHEN "NEU"      PERFORM KONTAKT-NEU
               WHEN "SPEICHER" PERFORM KONTAKT-SPEICHERN
               WHEN "ARCHIV"
                   MOVE "J" TO KT-ARCHIV
                   PERFORM KONTAKT-SPEICHERN
               WHEN "REAKTIV"
                   MOVE "N" TO KT-ARCHIV
                   PERFORM KONTAKT-SPEICHERN
               WHEN "TILGEN"   PERFORM KONTAKT-TILGEN
               WHEN "SUCHKEY"  PERFORM SUCHKEY-BILDEN
               WHEN "DUBLETTE" PERFORM DUBLETTE-SUCHEN
               WHEN "STDADR"
                   MOVE KT-NR TO W-NR
                   PERFORM STDADR-LESEN
                   MOVE W-GEFUNDEN TO DI-OK
                   MOVE WA-SATZ TO AD-SATZ
               WHEN "BEVKOMM"  PERFORM BEVKOMM-LESEN
               WHEN "ADR-NEU"  PERFORM ADRESSE-NEU
               WHEN "ADR-SPEI" PERFORM ADRESSE-SPEICHERN
               WHEN "ADR-LOE"  PERFORM ADRESSE-LOESCHEN
               WHEN "KOM-NEU"  PERFORM KOMM-NEU
               WHEN "KOM-SPEI" PERFORM KOMM-SPEICHERN
               WHEN "KOM-LOE"  PERFORM KOMM-LOESCHEN
               WHEN "ANSCHR"   PERFORM ANSCHRIFT-BILDEN
               WHEN OTHER
                   MOVE "N" TO DI-OK
                   MOVE "Unbekannte Funktion" TO DI-MELDUNG
           END-EVALUATE
           GOBACK.

      ******************************************************************
      * Kontakte
      ******************************************************************
       KONTAKT-NEU.
           CALL "NRVERG" USING KT-ART W-NR W-NV-STATUS
           IF W-NV-STATUS NOT = "00"
               MOVE "N" TO DI-OK
               STRING "Nummernvergabe fehlgeschlagen, Status "
                      W-NV-STATUS DELIMITED BY SIZE INTO DI-MELDUNG
               EXIT PARAGRAPH
           END-IF
           MOVE W-NR TO KT-NR
           MOVE W-JETZT TO KT-ANGELEGT KT-GEAENDERT
           IF KT-ARCHIV NOT = "J"
               MOVE "N" TO KT-ARCHIV
           END-IF
           PERFORM SUCHKEY-BILDEN
           MOVE "SCHREIB" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM IO-FEHLER-MELDEN.

       KONTAKT-SPEICHERN.
           MOVE W-JETZT TO KT-GEAENDERT
           PERFORM SUCHKEY-BILDEN
           MOVE "AENDERN" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM IO-FEHLER-MELDEN.

      * Entfernt den Kontakt mit allen Adressen und Kommunikations-
      * wegen. Bei einer Firma verlieren die Ansprechpartner nur ihre
      * Zuordnung, sie selbst bleiben erhalten.
       KONTAKT-TILGEN.
           MOVE KT-NR TO W-NR
           PERFORM ADRESSEN-LADEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ADR-ANZ
               MOVE W-ADR(W-I) TO WA-SATZ
               MOVE "LOESCHEN" TO IO-FUNKTION
               CALL "ADIO" USING IO-PARM WA-SATZ
           END-PERFORM
           PERFORM KOMM-LADEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-KOM-ANZ
               MOVE W-KOM(W-I) TO WO-SATZ
               MOVE "LOESCHEN" TO IO-FUNKTION
               CALL "KOIO" USING IO-PARM WO-SATZ
           END-PERFORM
           IF KT-IST-FIRMA
               PERFORM ANSPRECHPARTNER-LOESEN
           END-IF
           MOVE "LOESCHEN" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM IO-FEHLER-MELDEN.

       ANSPRECHPARTNER-LOESEN.
           MOVE 0 TO W-NR-ANZ
           MOVE W-NR TO WK-FIRMA-NR
           MOVE "START-FI" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM WK-SATZ
           PERFORM UNTIL NOT IO-OK OR W-NR-ANZ >= 2000
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM WK-SATZ
               IF IO-OK AND WK-FIRMA-NR = W-NR
                   ADD 1 TO W-NR-ANZ
                   MOVE WK-NR TO W-NR-E(W-NR-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-NR-ANZ
               MOVE W-NR-E(W-I) TO WK-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM WK-SATZ
               IF IO-OK
                   MOVE SPACES TO WK-FIRMA-NR
                   MOVE W-JETZT TO WK-GEAENDERT
                   MOVE "AENDERN" TO IO-FUNKTION
                   CALL "KTIO" USING IO-PARM WK-SATZ
               END-IF
           END-PERFORM.

      * Firma: Name. Person: Nachname, dann Vorname.
       SUCHKEY-BILDEN.
           MOVE SPACES TO W-SUCH-TEXT
           IF KT-IST-FIRMA
               MOVE KF-NAME TO W-SUCH-TEXT
           ELSE
               STRING FUNCTION TRIM(KP-NACHNAME) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(KP-VORNAME) DELIMITED BY SIZE
                 INTO W-SUCH-TEXT
           END-IF
           CALL "SUCHKEY" USING W-SUCH-TEXT KT-SUCH.

      * Gleicher Suchschlüssel und gleiche Art; die PLZ muss gleich
      * sein, sofern beide eine haben. DI-TREFFER-NR bleibt sonst leer.
       DUBLETTE-SUCHEN.
           MOVE SPACES TO DI-TREFFER-NR
           PERFORM SUCHKEY-BILDEN
           IF KT-SUCH = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE KT-SUCH TO WK-SUCH
           MOVE "START-SU" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM WK-SATZ
           PERFORM UNTIL NOT IO-OK OR DI-TREFFER-NR NOT = SPACES
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM WK-SATZ
               IF IO-OK AND WK-SUCH NOT = KT-SUCH
                   MOVE "10" TO IO-STATUS
               END-IF
               IF IO-OK AND WK-NR NOT = KT-NR AND WK-ART = KT-ART
                   MOVE WK-NR TO W-NR
                   PERFORM STDADR-LESEN
                   IF DI-PLZ = SPACES OR W-GEFUNDEN = "N"
                      OR WA-PLZ = SPACES OR WA-PLZ = DI-PLZ
                       MOVE WK-NR TO DI-TREFFER-NR
                       STRING "Möglicherweise schon vorhanden: "
                              DELIMITED BY SIZE
                              WK-NR DELIMITED BY SIZE
                         INTO DI-MELDUNG
                   END-IF
               END-IF
           END-PERFORM.

      ******************************************************************
      * Adressen
      ******************************************************************
       ADRESSEN-LADEN.
           MOVE 0 TO W-ADR-ANZ
           MOVE W-NR TO WA-KT-NR
           MOVE 0 TO WA-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM WA-SATZ
           PERFORM UNTIL NOT IO-OK OR W-ADR-ANZ >= 99
               MOVE "WEITER" TO IO-FUNKTION
               CALL "ADIO" USING IO-PARM WA-SATZ
               IF IO-OK AND WA-KT-NR = W-NR
                   ADD 1 TO W-ADR-ANZ
                   MOVE WA-SATZ TO W-ADR(W-ADR-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM.

      * Standardadresse von W-NR nach WA-SATZ, ersatzweise die erste.
       STDADR-LESEN.
           PERFORM ADRESSEN-LADEN
           MOVE SPACES TO WA-SATZ
           MOVE "N" TO W-GEFUNDEN
           IF W-ADR-ANZ > 0
               MOVE W-ADR(1) TO WA-SATZ
               MOVE "J" TO W-GEFUNDEN
           END-IF
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ADR-ANZ
               IF W-ADR(W-I)(11:1) = "J"
                   MOVE W-ADR(W-I) TO WA-SATZ
                   MOVE W-ADR-ANZ TO W-I
               END-IF
           END-PERFORM.

       ADRESSE-NEU.
           MOVE AD-KT-NR TO W-NR
           PERFORM ADRESSEN-LADEN
           MOVE 0 TO W-MAX-LFD
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ADR-ANZ
               MOVE W-ADR(W-I) TO WA-SATZ
               IF WA-LFD > W-MAX-LFD
                   MOVE WA-LFD TO W-MAX-LFD
               END-IF
           END-PERFORM
           IF W-MAX-LFD >= 99
               MOVE "N" TO DI-OK
               MOVE "Höchstens 99 Adressen je Kontakt" TO DI-MELDUNG
               EXIT PARAGRAPH
           END-IF
           COMPUTE AD-LFD = W-MAX-LFD + 1
           IF W-ADR-ANZ = 0
               MOVE "J" TO AD-STANDARD
           END-IF
           IF AD-STANDARD NOT = "J"
               MOVE "N" TO AD-STANDARD
           END-IF
           MOVE "SCHREIB" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM AD-SATZ
           PERFORM IO-FEHLER-MELDEN
           IF DI-ERFOLG
               PERFORM STANDARD-EINDEUTIG
           END-IF.

       ADRESSE-SPEICHERN.
           MOVE AD-KT-NR TO W-NR
           IF AD-STANDARD NOT = "J"
               MOVE "N" TO AD-STANDARD
           END-IF
           MOVE "AENDERN" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM AD-SATZ
           PERFORM IO-FEHLER-MELDEN
           IF DI-ERFOLG
               PERFORM STANDARD-EINDEUTIG
           END-IF.

       ADRESSE-LOESCHEN.
           MOVE AD-KT-NR TO W-NR
           MOVE "LOESCHEN" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM AD-SATZ
           PERFORM IO-FEHLER-MELDEN
           MOVE "N" TO AD-STANDARD
           IF DI-ERFOLG
               PERFORM STANDARD-EINDEUTIG
           END-IF.

      * Genau eine Standardadresse: Ist AD-SATZ Standard, verlieren
      * die anderen das Kennzeichen; hat keine es, bekommt es die erste.
       STANDARD-EINDEUTIG.
           PERFORM ADRESSEN-LADEN
           MOVE "N" TO W-GEFUNDEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ADR-ANZ
               MOVE W-ADR(W-I) TO WA-SATZ
               IF WA-IST-STANDARD
                   IF AD-IST-STANDARD AND WA-LFD NOT = AD-LFD
                       MOVE "N" TO WA-STANDARD
                       MOVE "AENDERN" TO IO-FUNKTION
                       CALL "ADIO" USING IO-PARM WA-SATZ
                   ELSE
                       MOVE "J" TO W-GEFUNDEN
                   END-IF
               END-IF
           END-PERFORM
           IF W-GEFUNDEN = "N" AND W-ADR-ANZ > 0
               MOVE W-ADR(1) TO WA-SATZ
               MOVE "J" TO WA-STANDARD
               MOVE "AENDERN" TO IO-FUNKTION
               CALL "ADIO" USING IO-PARM WA-SATZ
               IF WA-LFD = AD-LFD
                   MOVE "J" TO AD-STANDARD
               END-IF
           END-IF.

      ******************************************************************
      * Kommunikationswege
      ******************************************************************
       KOMM-LADEN.
           MOVE 0 TO W-KOM-ANZ
           MOVE W-NR TO WO-KT-NR
           MOVE 0 TO WO-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM WO-SATZ
           PERFORM UNTIL NOT IO-OK OR W-KOM-ANZ >= 99
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KOIO" USING IO-PARM WO-SATZ
               IF IO-OK AND WO-KT-NR = W-NR
                   ADD 1 TO W-KOM-ANZ
                   MOVE WO-SATZ TO W-KOM(W-KOM-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM.

      * Bevorzugter Eintrag vom Typ DI-KOMMTYP, sonst der erste.
       BEVKOMM-LESEN.
           MOVE KT-NR TO W-NR
           PERFORM KOMM-LADEN
           MOVE "N" TO DI-OK
           MOVE SPACES TO KO-SATZ
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-KOM-ANZ
               MOVE W-KOM(W-I) TO WO-SATZ
               IF WO-TYP = DI-KOMMTYP
                   IF DI-OK = "N" OR WO-IST-BEVORZUGT
                       MOVE WO-SATZ TO KO-SATZ
                   END-IF
                   MOVE "J" TO DI-OK
               END-IF
           END-PERFORM.

       KOMM-NEU.
           MOVE KO-KT-NR TO W-NR
           PERFORM KOMM-LADEN
           MOVE 0 TO W-MAX-LFD
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-KOM-ANZ
               MOVE W-KOM(W-I) TO WO-SATZ
               IF WO-LFD > W-MAX-LFD
                   MOVE WO-LFD TO W-MAX-LFD
               END-IF
           END-PERFORM
           IF W-MAX-LFD >= 99
               MOVE "N" TO DI-OK
               MOVE "Höchstens 99 Einträge je Kontakt" TO DI-MELDUNG
               EXIT PARAGRAPH
           END-IF
           COMPUTE KO-LFD = W-MAX-LFD + 1
           IF KO-BEVORZUGT NOT = "J"
               MOVE "N" TO KO-BEVORZUGT
           END-IF
           MOVE "SCHREIB" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM KO-SATZ
           PERFORM IO-FEHLER-MELDEN
           IF DI-ERFOLG
               PERFORM BEVORZUGT-EINDEUTIG
           END-IF.

       KOMM-SPEICHERN.
           MOVE KO-KT-NR TO W-NR
           IF KO-BEVORZUGT NOT = "J"
               MOVE "N" TO KO-BEVORZUGT
           END-IF
           MOVE "AENDERN" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM KO-SATZ
           PERFORM IO-FEHLER-MELDEN
           IF DI-ERFOLG
               PERFORM BEVORZUGT-EINDEUTIG
           END-IF.

       KOMM-LOESCHEN.
           MOVE KO-KT-NR TO W-NR
           MOVE "LOESCHEN" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM KO-SATZ
           PERFORM IO-FEHLER-MELDEN
           MOVE "N" TO KO-BEVORZUGT
           IF DI-ERFOLG
               PERFORM BEVORZUGT-EINDEUTIG
           END-IF.

      * Je Typ genau ein bevorzugter Eintrag (wie STANDARD-EINDEUTIG).
       BEVORZUGT-EINDEUTIG.
           PERFORM KOMM-LADEN
           MOVE KO-TYP TO W-TYP
           MOVE "N" TO W-GEFUNDEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-KOM-ANZ
               MOVE W-KOM(W-I) TO WO-SATZ
               IF WO-TYP = W-TYP AND WO-IST-BEVORZUGT
                   IF KO-IST-BEVORZUGT AND WO-LFD NOT = KO-LFD
                       MOVE "N" TO WO-BEVORZUGT
                       MOVE "AENDERN" TO IO-FUNKTION
                       CALL "KOIO" USING IO-PARM WO-SATZ
                   ELSE
                       MOVE "J" TO W-GEFUNDEN
                   END-IF
               END-IF
           END-PERFORM
           IF W-GEFUNDEN = "N"
               PERFORM VARYING W-I FROM 1 BY 1
                       UNTIL W-I > W-KOM-ANZ OR W-GEFUNDEN = "J"
                   MOVE W-KOM(W-I) TO WO-SATZ
                   IF WO-TYP = W-TYP
                       MOVE "J" TO WO-BEVORZUGT W-GEFUNDEN
                       MOVE "AENDERN" TO IO-FUNKTION
                       CALL "KOIO" USING IO-PARM WO-SATZ
                       IF WO-LFD = KO-LFD
                           MOVE "J" TO KO-BEVORZUGT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

      ******************************************************************
      * Anschrift: Ansprechpartner einer Firma ohne eigene Geschäfts-
      * adresse bekommen die Firmenzeilen und die Firmenadresse.
      ******************************************************************
       ANSCHRIFT-BILDEN.
           MOVE SPACES TO AN-FIRMA1 AN-FIRMA2
           MOVE KT-NR TO W-NR
           PERFORM STDADR-LESEN
           IF KT-IST-PERSON AND KT-FIRMA-NR NOT = SPACES
               MOVE KT-FIRMA-NR TO WK-NR
               MOVE "LESEN" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM WK-SATZ
               IF IO-OK
                   PERFORM FIRMENZEILEN-HOLEN
                   IF W-GEFUNDEN = "N" OR WA-TYP = "W"
                       MOVE WK-NR TO W-NR
                       PERFORM STDADR-LESEN
                   END-IF
               END-IF
           END-IF
           CALL "ANSCHR" USING AN-PARM KT-SATZ WA-SATZ
           MOVE AN-ANZAHL TO DI-ANZ-ZEILEN
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 6
               MOVE AN-ZEILE(W-I) TO DI-ZEILE(W-I)
           END-PERFORM
           CALL "BRIEFANR" USING KT-SATZ DI-BRIEFANR.

      * Die Firmenzeilen liefert ANSCHR selbst, mit leerer Adresse.
       FIRMENZEILEN-HOLEN.
           MOVE WA-SATZ TO W-ADR(99)
           MOVE SPACES TO WA-SATZ
           CALL "ANSCHR" USING AN-PARM WK-SATZ WA-SATZ
           MOVE AN-ZEILE(1) TO AN-FIRMA1
           IF AN-ANZAHL > 1
               MOVE AN-ZEILE(2) TO AN-FIRMA2
           END-IF
           MOVE W-ADR(99) TO WA-SATZ.

       IO-FEHLER-MELDEN.
           IF NOT IO-OK
               MOVE "N" TO DI-OK
               STRING "Dateifehler, Status " IO-STATUS
                      DELIMITED BY SIZE INTO DI-MELDUNG
           END-IF.
