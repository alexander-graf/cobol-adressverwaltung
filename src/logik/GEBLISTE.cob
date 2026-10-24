       IDENTIFICATION DIVISION.
       PROGRAM-ID. GEBLISTE.
      * Personen mit Geburtstag in den nächsten GL-TAGE Tagen ab
      * GL-HEUTE, nach Abstand sortiert. Am 29.02. Geborene feiern in
      * Nicht-Schaltjahren am 28.02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY kontakt.
       COPY wort-d.
       01  W-HEUTE-JAHR            PIC 9(4).
       01  W-HEUTE-INT             PIC 9(8).
       01  W-JAHR                  PIC 9(4).
       01  W-TERMIN.
           05  W-T-JAHR            PIC 9(4).
           05  W-T-MMTT            PIC 9(4).
       01  W-TERMIN-N REDEFINES W-TERMIN PIC 9(8).
       01  W-ABSTAND               PIC S9(8).
       01  W-I                     PIC 999.
       01  W-J                     PIC 999.
       01  W-TAUSCH                PIC X(71).
       LINKAGE SECTION.
       COPY geb.
       PROCEDURE DIVISION USING GL-PARM.
           MOVE 0 TO GL-ANZAHL
           MOVE GL-HEUTE(1:4) TO W-HEUTE-JAHR
           COMPUTE W-HEUTE-INT = FUNCTION INTEGER-OF-DATE(GL-HEUTE)
           MOVE LOW-VALUES TO KT-NR
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM UNTIL NOT IO-OK
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK AND KT-IST-PERSON AND NOT KT-ARCHIVIERT
                  AND KP-GEBDATUM NOT = 0
                   PERFORM PERSON-PRUEFEN
               END-IF
           END-PERFORM
           PERFORM SORTIEREN
           GOBACK.

       PERSON-PRUEFEN.
           MOVE W-HEUTE-JAHR TO W-JAHR
           PERFORM TERMIN-BILDEN
           IF W-TERMIN-N < GL-HEUTE
               ADD 1 TO W-JAHR
               PERFORM TERMIN-BILDEN
           END-IF
           COMPUTE W-ABSTAND = FUNCTION INTEGER-OF-DATE(W-TERMIN-N)
                             - W-HEUTE-INT
           IF W-ABSTAND <= GL-TAGE AND GL-ANZAHL < 200
               ADD 1 TO GL-ANZAHL
               MOVE KT-NR TO GL-NR(GL-ANZAHL)
               MOVE KP-GEBDATUM TO GL-GEBDATUM(GL-ANZAHL)
               MOVE W-ABSTAND TO GL-IN-TAGEN(GL-ANZAHL)
               COMPUTE GL-WIRD(GL-ANZAHL) =
                   W-JAHR - FUNCTION INTEGER(KP-GEBDATUM / 10000)
               PERFORM SATZ-LEEREN
               MOVE KP-TITEL TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE KP-VORNAME TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE KP-VORSATZ TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE KP-NACHNAME TO W-WORT
               PERFORM WORT-ANHAENGEN
               MOVE W-SATZ TO GL-NAME(GL-ANZAHL)
           END-IF.

       TERMIN-BILDEN.
           MOVE W-JAHR TO W-T-JAHR
           COMPUTE W-T-MMTT = FUNCTION MOD(KP-GEBDATUM, 10000)
           IF W-T-MMTT = 0229
              AND NOT (FUNCTION MOD(W-JAHR, 4) = 0
                       AND (FUNCTION MOD(W-JAHR, 100) NOT = 0
                            OR FUNCTION MOD(W-JAHR, 400) = 0))
               MOVE 0228 TO W-T-MMTT
           END-IF.

      * Einfügesortierung nach GL-IN-TAGEN, die Liste ist kurz.
       SORTIEREN.
           PERFORM VARYING W-I FROM 2 BY 1 UNTIL W-I > GL-ANZAHL
               MOVE GL-EINTRAG(W-I) TO W-TAUSCH
               MOVE W-I TO W-J
               PERFORM UNTIL W-J < 2
                   OR GL-IN-TAGEN(W-J - 1) <= W-TAUSCH(66:3)
                   MOVE GL-EINTRAG(W-J - 1) TO GL-EINTRAG(W-J)
                   SUBTRACT 1 FROM W-J
               END-PERFORM
               MOVE W-TAUSCH TO GL-EINTRAG(W-J)
           END-PERFORM.

       COPY wort-p.
