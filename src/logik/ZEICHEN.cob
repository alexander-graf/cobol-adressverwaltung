       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZEICHEN.
      * Wandelt Text zwischen ISO-8859-15 (intern) und UTF-8 (Dateien
      * nach außen). Zeichen, die Latin-9 nicht kennt, werden zu "?"
      * und in ZC-ERSETZT gezählt; typografische Anführungszeichen und
      * Striche werden vorher durch ihre ASCII-Form ersetzt.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-I                     PIC 9(5).
       01  W-B                     PIC 999.
       01  W-B2                    PIC 999.
       01  W-B3                    PIC 999.
       01  W-CODE                  PIC 9(5).
       01  W-NEU                   PIC X(3).
       01  W-NEU-LEN               PIC 9.
       01  W-K                     PIC 9.
       LINKAGE SECTION.
       COPY zeichen.
       PROCEDURE DIVISION USING ZC-PARM.
           MOVE 0 TO ZC-AUS-LEN ZC-ERSETZT
           MOVE SPACES TO ZC-AUS
           MOVE 1 TO W-I
           PERFORM UNTIL W-I > ZC-EIN-LEN
               COMPUTE W-B = FUNCTION ORD(ZC-EIN(W-I:1)) - 1
               IF ZC-RICHTUNG = "U"
                   PERFORM NACH-UTF8
               ELSE
                   PERFORM NACH-LATIN9
               END-IF
               PERFORM VARYING W-K FROM 1 BY 1 UNTIL W-K > W-NEU-LEN
                   IF ZC-AUS-LEN < LENGTH OF ZC-AUS
                       ADD 1 TO ZC-AUS-LEN
                       MOVE W-NEU(W-K:1) TO ZC-AUS(ZC-AUS-LEN:1)
                   END-IF
               END-PERFORM
           END-PERFORM
           GOBACK.

       NACH-UTF8.
           MOVE 1 TO W-NEU-LEN
           EVALUATE TRUE
               WHEN W-B < 128
                   MOVE ZC-EIN(W-I:1) TO W-NEU
               WHEN W-B < 160
                   MOVE "?" TO W-NEU
                   ADD 1 TO ZC-ERSETZT
               WHEN W-B = 164
                   MOVE X"E282AC" TO W-NEU
                   MOVE 3 TO W-NEU-LEN
               WHEN W-B = 166 OR 168 OR 180 OR 184
                 OR 188 OR 189 OR 190
                   MOVE 2 TO W-NEU-LEN
                   EVALUATE W-B
                       WHEN 166 MOVE X"C5A0" TO W-NEU
                       WHEN 168 MOVE X"C5A1" TO W-NEU
                       WHEN 180 MOVE X"C5BD" TO W-NEU
                       WHEN 184 MOVE X"C5BE" TO W-NEU
                       WHEN 188 MOVE X"C592" TO W-NEU
                       WHEN 189 MOVE X"C593" TO W-NEU
                       WHEN 190 MOVE X"C5B8" TO W-NEU
                   END-EVALUATE
               WHEN W-B < 192
                   MOVE 2 TO W-NEU-LEN
                   MOVE X"C2" TO W-NEU(1:1)
                   MOVE ZC-EIN(W-I:1) TO W-NEU(2:1)
               WHEN OTHER
                   MOVE 2 TO W-NEU-LEN
                   MOVE X"C3" TO W-NEU(1:1)
                   MOVE FUNCTION CHAR(W-B - 64 + 1) TO W-NEU(2:1)
           END-EVALUATE
           ADD 1 TO W-I.

       NACH-LATIN9.
           MOVE 1 TO W-NEU-LEN
           MOVE "?" TO W-NEU
           MOVE 0 TO W-B2 W-B3
           IF W-I + 1 <= ZC-EIN-LEN
               COMPUTE W-B2 = FUNCTION ORD(ZC-EIN(W-I + 1:1)) - 1
           END-IF
           IF W-I + 2 <= ZC-EIN-LEN
               COMPUTE W-B3 = FUNCTION ORD(ZC-EIN(W-I + 2:1)) - 1
           END-IF
           EVALUATE TRUE
               WHEN W-B < 128
                   MOVE ZC-EIN(W-I:1) TO W-NEU
                   ADD 1 TO W-I
               WHEN W-B >= 194 AND W-B <= 197
                AND W-B2 >= 128 AND W-B2 < 192
                   COMPUTE W-CODE = (W-B - 192) * 64 + W-B2 - 128
                   PERFORM CODE-NACH-LATIN9
                   ADD 2 TO W-I
               WHEN W-B = 226 AND W-B2 = 130 AND W-B3 = 172
                   MOVE X"A4" TO W-NEU
                   ADD 3 TO W-I
               WHEN W-B = 226 AND W-B2 = 128
                   EVALUATE W-B3
                       WHEN 147 WHEN 148
                           MOVE "-" TO W-NEU
                       WHEN 152 WHEN 153 WHEN 154
                           MOVE "'" TO W-NEU
                       WHEN 156 WHEN 157 WHEN 158
                           MOVE QUOTE TO W-NEU
                       WHEN OTHER
                           ADD 1 TO ZC-ERSETZT
                   END-EVALUATE
                   ADD 3 TO W-I
               WHEN W-B >= 240
                   ADD 1 TO ZC-ERSETZT
                   ADD 4 TO W-I
               WHEN W-B >= 224
                   ADD 1 TO ZC-ERSETZT
                   ADD 3 TO W-I
               WHEN W-B >= 192
                   ADD 1 TO ZC-ERSETZT
                   ADD 2 TO W-I
               WHEN OTHER
      *            Folgebyte ohne Startbyte: kaputtes UTF-8
                   ADD 1 TO ZC-ERSETZT
                   ADD 1 TO W-I
           END-EVALUATE.

      * Unicode-Codepunkt (bis U+017F) -> Latin-9-Byte
       CODE-NACH-LATIN9.
           EVALUATE W-CODE
               WHEN 352 MOVE X"A6" TO W-NEU
               WHEN 353 MOVE X"A8" TO W-NEU
               WHEN 381 MOVE X"B4" TO W-NEU
               WHEN 382 MOVE X"B8" TO W-NEU
               WHEN 338 MOVE X"BC" TO W-NEU
               WHEN 339 MOVE X"BD" TO W-NEU
               WHEN 376 MOVE X"BE" TO W-NEU
               WHEN 164 WHEN 166 WHEN 168 WHEN 180
               WHEN 184 WHEN 188 WHEN 189 WHEN 190
                   ADD 1 TO ZC-ERSETZT
               WHEN 160 THRU 255
                   MOVE FUNCTION CHAR(W-CODE + 1) TO W-NEU
               WHEN OTHER
                   ADD 1 TO ZC-ERSETZT
           END-EVALUATE.
