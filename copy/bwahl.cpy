      * Parameter für die Trefferliste BWAHL.
      * BW-MODUS: S = Suche (F5 Neu, F11 Etiketten), A = nur Auswahl
      * BW-RUECK: WAHL (BW-NR gewählt), NEU, ETIK, ZURUECK
       01  BW-PARM.
           05  BW-TITEL            PIC X(50).
           05  BW-MODUS            PIC X.
           05  BW-RUECK            PIC X(8).
           05  BW-NR               PIC X(7).
           05  BW-MELDUNG          PIC X(79).
