      * Parameter für KTDIENST.
      * Funktionen: NEU SPEICHER ARCHIV REAKTIV TILGEN SUCHKEY
      *   DUBLETTE STDADR BEVKOMM ADR-NEU ADR-SPEI ADR-LOE
      *   KOM-NEU KOM-SPEI KOM-LOE ANSCHR
       01  DI-PARM.
           05  DI-FUNKTION         PIC X(8).
           05  DI-OK               PIC X.
               88  DI-ERFOLG       VALUE "J".
           05  DI-MELDUNG          PIC X(70).
           05  DI-PLZ              PIC X(10).
           05  DI-TREFFER-NR       PIC X(7).
           05  DI-KOMMTYP          PIC XX.
           05  DI-ANZ-ZEILEN       PIC 9.
           05  DI-ZEILE            PIC X(60) OCCURS 6.
           05  DI-BRIEFANR         PIC X(80).
