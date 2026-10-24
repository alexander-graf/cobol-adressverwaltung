      * Kommunikationsweg: beliebig viele je Kontakt.
      * KO-TYP siehe Liste KOMMTYP im Modul CODES.
       01  KO-SATZ.
           05  KO-SCHLUESSEL.
               10  KO-KT-NR        PIC X(7).
               10  KO-LFD          PIC 99.
           05  KO-TYP              PIC XX.
           05  KO-WERT             PIC X(100).
           05  KO-BEZ              PIC X(20).
           05  KO-BEVORZUGT        PIC X.
               88  KO-IST-BEVORZUGT VALUE "J".
