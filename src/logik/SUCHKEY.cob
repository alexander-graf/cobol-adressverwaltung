       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUCHKEY.
      * Normalisiert einen Text (Latin-9) zum Suchschlüssel:
      * Großbuchstaben, Umlaute ausgeschrieben (Ä -> AE, ß -> SS),
      * Akzente entfernt, alles andere wird zu einem Leerzeichen.
      * "Müller-Lüdenscheidt" -> "MUELLER LUEDENSCHEIDT"
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-I                     PIC 9(3).
       01  W-Z                     PIC X.
       01  W-NEU                   PIC XX.
       01  W-NEU-LEN               PIC 9.
       01  W-POS                   PIC 9(3).
       01  W-K                     PIC 9.
       01  W-KLEIN    PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
       01  W-GROSS    PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       LINKAGE SECTION.
       01  SK-EIN                  PIC X(100).
       01  SK-AUS                  PIC X(40).
       PROCEDURE DIVISION USING SK-EIN SK-AUS.
           MOVE SPACES TO SK-AUS
           MOVE 0 TO W-POS
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > 100 OR W-POS >= 40
               MOVE SK-EIN(W-I:1) TO W-Z
               INSPECT W-Z CONVERTING W-KLEIN TO W-GROSS
               PERFORM ZEICHEN-WANDELN
               IF W-NEU-LEN > 0
                   PERFORM VARYING W-K FROM 1 BY 1
                           UNTIL W-K > W-NEU-LEN OR W-POS >= 40
                       ADD 1 TO W-POS
                       MOVE W-NEU(W-K:1) TO SK-AUS(W-POS:1)
                   END-PERFORM
               END-IF
           END-PERFORM
           GOBACK.

       ZEICHEN-WANDELN.
           MOVE SPACES TO W-NEU
           MOVE 1 TO W-NEU-LEN
           EVALUATE W-Z
               WHEN "A" THRU "Z"
               WHEN "0" THRU "9"
                   MOVE W-Z TO W-NEU
               WHEN X"E4" WHEN X"C4" WHEN X"E6" WHEN X"C6"
                   MOVE "AE" TO W-NEU
               WHEN X"F6" WHEN X"D6" WHEN X"BC" WHEN X"BD"
                   MOVE "OE" TO W-NEU
               WHEN X"FC" WHEN X"DC"
                   MOVE "UE" TO W-NEU
               WHEN X"DF"
                   MOVE "SS" TO W-NEU
               WHEN X"E0" THRU X"E3" WHEN X"E5"
               WHEN X"C0" THRU X"C3" WHEN X"C5"
                   MOVE "A" TO W-NEU
               WHEN X"E7" WHEN X"C7"
                   MOVE "C" TO W-NEU
               WHEN X"E8" THRU X"EB" WHEN X"C8" THRU X"CB"
                   MOVE "E" TO W-NEU
               WHEN X"EC" THRU X"EF" WHEN X"CC" THRU X"CF"
                   MOVE "I" TO W-NEU
               WHEN X"F1" WHEN X"D1"
                   MOVE "N" TO W-NEU
               WHEN X"F2" THRU X"F5" WHEN X"F8"
               WHEN X"D2" THRU X"D5" WHEN X"D8"
                   MOVE "O" TO W-NEU
               WHEN X"F9" THRU X"FB" WHEN X"D9" THRU X"DB"
                   MOVE "U" TO W-NEU
               WHEN X"FD" WHEN X"FF" WHEN X"DD" WHEN X"BE"
                   MOVE "Y" TO W-NEU
               WHEN X"A6" WHEN X"A8"
                   MOVE "S" TO W-NEU
               WHEN X"B4" WHEN X"B8"
                   MOVE "Z" TO W-NEU
               WHEN OTHER
      *            Trenner: höchstens ein Leerzeichen, nie am Anfang
                   IF W-POS = 0 OR SK-AUS(W-POS:1) = SPACE
                       MOVE 0 TO W-NEU-LEN
                   END-IF
           END-EVALUATE
           IF W-NEU(2:1) NOT = SPACE
               MOVE 2 TO W-NEU-LEN
           END-IF.
