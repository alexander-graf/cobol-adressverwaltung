       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANSCHR.
      * Anschriftzone nach DIN 5008, höchstens 6 Zeilen:
      *   [Firma] [Firmenzusatz] [Person] [Adresszusatz]
      *   Straße Hausnr. | Postfach   PLZ Ort   [LAND bei Ausland]
      * Wird es zu lang, fallen zuerst Firmenzusatz, dann
      * Adresszusatz weg.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY wort-d.
       COPY codes.
       01  W-ZEILEN.
           05  W-Z                 OCCURS 8.
               10  W-Z-TEXT        PIC X(60).
               10  W-Z-RANG        PIC 9.
       01  W-ANZ                   PIC 9.
       01  W-I                     PIC 9.
       01  W-WEG                   PIC 9.
       01  W-RF-LEN                PIC 99.
       01  W-NAME-LEN              PIC 99.
       01  W-KLEIN PIC X(29) VALUE "abcdefghijklmnopqrstuvwxyzäöü".
       01  W-GROSS PIC X(29) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ".
       LINKAGE SECTION.
       COPY anschr.
       COPY kontakt.
       COPY adresse.
       PROCEDURE DIVISION USING AN-PARM KT-SATZ AD-SATZ.
           INITIALIZE W-ZEILEN
           MOVE 0 TO W-ANZ
           IF KT-IST-FIRMA
               PERFORM FIRMENZEILEN
           ELSE
               IF AN-FIRMA1 NOT = SPACES
                   MOVE AN-FIRMA1 TO W-SATZ
                   PERFORM ZEILE-FEST
                   IF AN-FIRMA2 NOT = SPACES
                       MOVE AN-FIRMA2 TO W-SATZ
                       PERFORM ZEILE-OPTIONAL-1
                   END-IF
               END-IF
               PERFORM PERSONENZEILE
           END-IF
           IF AD-ZUSATZ NOT = SPACES
               MOVE AD-ZUSATZ TO W-SATZ
               PERFORM ZEILE-OPTIONAL-2
           END-IF
           PERFORM STRASSENZEILE
           PERFORM ORTSZEILEN
           PERFORM KUERZEN
           MOVE 0 TO AN-ANZAHL
           MOVE SPACES TO AN-ZEILE(1) AN-ZEILE(2) AN-ZEILE(3)
                          AN-ZEILE(4) AN-ZEILE(5) AN-ZEILE(6)
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-ANZ
               IF W-Z-RANG(W-I) NOT = 9 AND AN-ANZAHL < 6
                   ADD 1 TO AN-ANZAHL
                   MOVE W-Z-TEXT(W-I) TO AN-ZEILE(AN-ANZAHL)
               END-IF
           END-PERFORM
           GOBACK.

       FIRMENZEILEN.
           PERFORM SATZ-LEEREN
           MOVE KF-NAME TO W-WORT
           PERFORM WORT-ANHAENGEN
      *    Rechtsform anhängen, wenn sie nicht schon im Namen steht
           IF KF-RECHTSFORM NOT = SPACES
              AND KF-RECHTSFORM NOT = "Einzelunternehmen"
              AND KF-RECHTSFORM NOT = "Freiberufler"
               MOVE FUNCTION LENGTH(FUNCTION TRIM(KF-RECHTSFORM))
                 TO W-RF-LEN
               MOVE W-SATZ-POS TO W-NAME-LEN
               MOVE KF-RECHTSFORM TO W-WORT
               IF W-NAME-LEN < W-RF-LEN
                   PERFORM WORT-ANHAENGEN
               ELSE
                   IF W-SATZ(W-NAME-LEN - W-RF-LEN + 1:W-RF-LEN)
                      NOT = KF-RECHTSFORM(1:W-RF-LEN)
                       PERFORM WORT-ANHAENGEN
                   END-IF
               END-IF
           END-IF
           PERFORM ZEILE-FEST
           IF KF-ZUSATZ NOT = SPACES
               MOVE KF-ZUSATZ TO W-SATZ
               PERFORM ZEILE-OPTIONAL-1
           END-IF.

       PERSONENZEILE.
           PERFORM SATZ-LEEREN
           EVALUATE KP-ANREDE
               WHEN "H" MOVE "Herrn" TO W-WORT
               WHEN "F" MOVE "Frau" TO W-WORT
               WHEN OTHER MOVE SPACES TO W-WORT
           END-EVALUATE
           PERFORM WORT-ANHAENGEN
           MOVE KP-TITEL TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-VORNAME TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-VORSATZ TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-NACHNAME TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-ZUSATZ TO W-WORT
           PERFORM WORT-ANHAENGEN
           PERFORM ZEILE-FEST.

       STRASSENZEILE.
           PERFORM SATZ-LEEREN
           IF AD-TYP = "P" AND AD-STRASSE(1:8) NOT = "Postfach"
               MOVE "Postfach" TO W-WORT
               PERFORM WORT-ANHAENGEN
           END-IF
           MOVE AD-STRASSE TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE AD-HAUSNR TO W-WORT
           PERFORM WORT-ANHAENGEN
           IF W-SATZ NOT = SPACES
               PERFORM ZEILE-FEST
           END-IF.

       ORTSZEILEN.
           PERFORM SATZ-LEEREN
           MOVE AD-PLZ TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE AD-ORT TO W-WORT
           PERFORM WORT-ANHAENGEN
           IF W-SATZ NOT = SPACES
               PERFORM ZEILE-FEST
           END-IF
           IF AD-LAND NOT = SPACES AND AD-LAND NOT = "DE"
               MOVE "TEXT" TO CD-FUNKTION
               MOVE "LAND" TO CD-LISTE
               MOVE AD-LAND TO CD-CODE
               CALL "CODES" USING CD-PARM
               IF CD-GUELTIG
                   MOVE CD-TEXT TO W-SATZ
               ELSE
                   MOVE AD-LAND TO W-SATZ
               END-IF
               INSPECT W-SATZ CONVERTING W-KLEIN TO W-GROSS
               PERFORM ZEILE-FEST
           END-IF.

      * Rang 0 = bleibt immer, 1 = fällt zuerst weg, 2 = danach
       ZEILE-FEST.
           ADD 1 TO W-ANZ
           MOVE W-SATZ TO W-Z-TEXT(W-ANZ)
           MOVE 0 TO W-Z-RANG(W-ANZ).

       ZEILE-OPTIONAL-1.
           PERFORM ZEILE-FEST
           MOVE 1 TO W-Z-RANG(W-ANZ).

       ZEILE-OPTIONAL-2.
           PERFORM ZEILE-FEST
           MOVE 2 TO W-Z-RANG(W-ANZ).

      * Zu viele Zeilen: Rang 1, dann Rang 2 ausblenden (Rang 9)
       KUERZEN.
           MOVE W-ANZ TO W-WEG
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > W-ANZ OR W-WEG <= 6
               IF W-Z-RANG(W-I) = 1
                   MOVE 9 TO W-Z-RANG(W-I)
                   SUBTRACT 1 FROM W-WEG
               END-IF
           END-PERFORM
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > W-ANZ OR W-WEG <= 6
               IF W-Z-RANG(W-I) = 2
                   MOVE 9 TO W-Z-RANG(W-I)
                   SUBTRACT 1 FROM W-WEG
               END-IF
           END-PERFORM.

       COPY wort-p.
