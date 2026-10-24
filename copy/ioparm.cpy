      * Parameter für die Dateimodule KTIO, ADIO, KOIO.
      * Funktionen: LESEN SCHREIB AENDERN LOESCHEN START-NR WEITER
      *             SCHLIESS; nur KTIO: START-SU START-FI
       01  IO-PARM.
           05  IO-FUNKTION         PIC X(8).
           05  IO-STATUS           PIC XX.
               88  IO-OK           VALUE "00" "02" "05".
               88  IO-ENDE         VALUE "10" "23" "46".
