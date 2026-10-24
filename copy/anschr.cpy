      * Anschrift nach DIN 5008 (Modul ANSCHR).
      * AN-FIRMA1/2: Firmenzeilen über einem Ansprechpartner.
       01  AN-PARM.
           05  AN-FIRMA1           PIC X(60).
           05  AN-FIRMA2           PIC X(60).
           05  AN-ANZAHL           PIC 9.
           05  AN-ZEILE            PIC X(60) OCCURS 6.
