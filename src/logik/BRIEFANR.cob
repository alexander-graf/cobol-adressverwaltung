       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRIEFANR.
      * Briefanrede aus der Anrede einer Person:
      *   H -> "Sehr geehrter Herr Dr. Mustermann,"
      *   F -> "Sehr geehrte Frau Dr. Mustermann,"
      *   N, K, leer -> "Guten Tag Kim Mustermann,"
      * Firma -> "Sehr geehrte Damen und Herren,"
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY wort-d.
       LINKAGE SECTION.
       COPY kontakt.
       01  BA-TEXT                 PIC X(80).
       PROCEDURE DIVISION USING KT-SATZ BA-TEXT.
           PERFORM SATZ-LEEREN
           IF KT-IST-FIRMA
               MOVE "Sehr geehrte Damen und Herren," TO W-SATZ
           ELSE
               EVALUATE KP-ANREDE
                   WHEN "H"
                       MOVE "Sehr geehrter Herr" TO W-WORT
                       PERFORM WORT-ANHAENGEN
                       PERFORM TITEL-UND-NAME
                   WHEN "F"
                       MOVE "Sehr geehrte Frau" TO W-WORT
                       PERFORM WORT-ANHAENGEN
                       PERFORM TITEL-UND-NAME
                   WHEN OTHER
                       MOVE "Guten Tag" TO W-WORT
                       PERFORM WORT-ANHAENGEN
                       MOVE KP-VORNAME TO W-WORT
                       PERFORM WORT-ANHAENGEN
                       MOVE KP-VORSATZ TO W-WORT
                       PERFORM WORT-ANHAENGEN
                       MOVE KP-NACHNAME TO W-WORT
                       PERFORM WORT-ANHAENGEN
               END-EVALUATE
               IF W-SATZ-POS < 80
                   MOVE "," TO W-SATZ(W-SATZ-POS + 1:1)
               END-IF
           END-IF
           MOVE W-SATZ TO BA-TEXT
           GOBACK.

       TITEL-UND-NAME.
           MOVE KP-TITEL TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-VORSATZ TO W-WORT
           PERFORM WORT-ANHAENGEN
           MOVE KP-NACHNAME TO W-WORT
           PERFORM WORT-ANHAENGEN.

       COPY wort-p.
