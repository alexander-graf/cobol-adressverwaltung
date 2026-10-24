      * Hängt W-WORT ohne Randleerzeichen an W-SATZ an, getrennt durch
      * ein Leerzeichen. Vorher SATZ-LEEREN ausführen.
       SATZ-LEEREN.
           MOVE SPACES TO W-SATZ
           MOVE 0 TO W-SATZ-POS.

       WORT-ANHAENGEN.
           IF W-WORT NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(W-WORT))
                 TO W-WORT-LEN
               IF W-SATZ-POS > 0
                   ADD 1 TO W-SATZ-POS
               END-IF
               IF W-SATZ-POS + W-WORT-LEN <= LENGTH OF W-SATZ
                   MOVE FUNCTION TRIM(W-WORT)
                     TO W-SATZ(W-SATZ-POS + 1:W-WORT-LEN)
                   ADD W-WORT-LEN TO W-SATZ-POS
               END-IF
           END-IF.
