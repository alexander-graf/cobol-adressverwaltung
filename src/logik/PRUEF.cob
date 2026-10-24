       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEF.
      * Feldprüfungen und Datumsumwandlung. Ein leeres Feld ist immer
      * gültig; ob ein Feld Pflicht ist, entscheidet der Aufrufer.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-TEXT                  PIC X(100).
       01  W-LEN                   PIC 9(3).
       01  W-I                     PIC 9(3).
       01  W-AT-POS                PIC 9(3).
       01  W-ANZ                   PIC 9(3).
       01  W-PUNKT                 PIC 9(3).
       01  W-JJJJMMTT.
           05  W-JJJJ              PIC 9(4).
           05  W-MM                PIC 99.
           05  W-TT                PIC 99.
       01  W-JJJJMMTT-N REDEFINES W-JJJJMMTT PIC 9(8).
       01  W-HEUTE                 PIC 9(8).
       LINKAGE SECTION.
       COPY pruef.
       PROCEDURE DIVISION USING PR-PARM.
           MOVE "J" TO PR-OK
           MOVE SPACES TO PR-MELDUNG
           MOVE FUNCTION TRIM(PR-WERT) TO W-TEXT
           IF W-TEXT = SPACES
               MOVE 0 TO W-LEN
           ELSE
               MOVE FUNCTION LENGTH(FUNCTION TRIM(PR-WERT)) TO W-LEN
           END-IF
           EVALUATE PR-FUNKTION
               WHEN "PLZ"     PERFORM PRUEF-PLZ
               WHEN "EMAIL"   PERFORM PRUEF-EMAIL
               WHEN "USTID"   PERFORM PRUEF-USTID
               WHEN "LAND"    PERFORM PRUEF-LAND
               WHEN "DATUM"   PERFORM PRUEF-DATUM
               WHEN "GEBDAT"  PERFORM PRUEF-GEBDAT
               WHEN "DATTEXT" PERFORM DATUM-ALS-TEXT
               WHEN OTHER
                   MOVE "N" TO PR-OK
                   MOVE "Unbekannte Prüfung" TO PR-MELDUNG
           END-EVALUATE
           GOBACK.

       PRUEF-PLZ.
           IF W-LEN = 0
               EXIT PARAGRAPH
           END-IF
           EVALUATE PR-LAND
               WHEN "DE"
               WHEN SPACES
                   IF W-LEN NOT = 5 OR W-TEXT(1:5) NOT NUMERIC
                       MOVE "N" TO PR-OK
                       MOVE "PLZ in Deutschland: 5 Ziffern"
                         TO PR-MELDUNG
                   END-IF
               WHEN "AT"
               WHEN "CH"
               WHEN "LI"
                   IF W-LEN NOT = 4 OR W-TEXT(1:4) NOT NUMERIC
                       MOVE "N" TO PR-OK
                       MOVE "PLZ in AT/CH/LI: 4 Ziffern" TO PR-MELDUNG
                   END-IF
           END-EVALUATE.

       PRUEF-EMAIL.
           IF W-LEN = 0
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO W-ANZ W-AT-POS W-PUNKT
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-LEN
               EVALUATE W-TEXT(W-I:1)
                   WHEN "@"
                       ADD 1 TO W-ANZ
                       MOVE W-I TO W-AT-POS
                   WHEN "."
                       IF W-AT-POS > 0 AND W-I > W-AT-POS + 1
                           MOVE W-I TO W-PUNKT
                       END-IF
                   WHEN SPACE
                       ADD 10 TO W-ANZ
               END-EVALUATE
           END-PERFORM
           IF W-ANZ NOT = 1 OR W-AT-POS < 2 OR W-PUNKT = 0
              OR W-PUNKT = W-LEN
               MOVE "N" TO PR-OK
               MOVE "E-Mail-Adresse sieht nicht gültig aus"
                 TO PR-MELDUNG
           END-IF.

       PRUEF-USTID.
           IF W-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF W-TEXT(1:2) IS NOT ALPHABETIC-UPPER OR W-LEN < 4
               MOVE "N" TO PR-OK
           ELSE
               EVALUATE W-TEXT(1:2)
                   WHEN "DE"
                       IF W-LEN NOT = 11
                          OR W-TEXT(3:9) NOT NUMERIC
                           MOVE "N" TO PR-OK
                       END-IF
                   WHEN "AT"
                       IF W-LEN NOT = 11 OR W-TEXT(3:1) NOT = "U"
                          OR W-TEXT(4:8) NOT NUMERIC
                           MOVE "N" TO PR-OK
                       END-IF
                   WHEN OTHER
                       IF W-LEN > 14
                           MOVE "N" TO PR-OK
                       END-IF
               END-EVALUATE
           END-IF
           IF PR-OK = "N"
               MOVE "USt-IdNr. z. B. DE123456789 oder ATU12345678"
                 TO PR-MELDUNG
           END-IF.

       PRUEF-LAND.
           IF W-LEN NOT = 0
              AND (W-LEN NOT = 2
                   OR W-TEXT(1:2) IS NOT ALPHABETIC-UPPER)
               MOVE "N" TO PR-OK
               MOVE "Land als ISO-Code, z. B. DE, AT, CH"
                 TO PR-MELDUNG
           END-IF.

       PRUEF-DATUM.
           MOVE 0 TO PR-DATUM
           IF W-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF W-LEN NOT = 10 OR W-TEXT(3:1) NOT = "."
              OR W-TEXT(6:1) NOT = "."
              OR W-TEXT(1:2) NOT NUMERIC OR W-TEXT(4:2) NOT NUMERIC
              OR W-TEXT(7:4) NOT NUMERIC
               MOVE "N" TO PR-OK
           ELSE
               MOVE W-TEXT(1:2) TO W-TT
               MOVE W-TEXT(4:2) TO W-MM
               MOVE W-TEXT(7:4) TO W-JJJJ
               IF FUNCTION TEST-DATE-YYYYMMDD(W-JJJJMMTT-N) NOT = 0
                   MOVE "N" TO PR-OK
               ELSE
                   MOVE W-JJJJMMTT-N TO PR-DATUM
               END-IF
           END-IF
           IF PR-OK = "N"
               MOVE "Datum als TT.MM.JJJJ" TO PR-MELDUNG
           END-IF.

       PRUEF-GEBDAT.
           PERFORM PRUEF-DATUM
           MOVE FUNCTION CURRENT-DATE(1:8) TO W-HEUTE
           IF PR-OK = "J" AND PR-DATUM > W-HEUTE
               MOVE "N" TO PR-OK
               MOVE 0 TO PR-DATUM
               MOVE "Geburtsdatum liegt in der Zukunft" TO PR-MELDUNG
           END-IF.

       DATUM-ALS-TEXT.
           MOVE SPACES TO PR-WERT
           IF PR-DATUM NOT = 0
               MOVE PR-DATUM TO W-JJJJMMTT-N
               STRING W-TT "." W-MM "." W-JJJJ DELIMITED BY SIZE
                 INTO PR-WERT
           END-IF.
