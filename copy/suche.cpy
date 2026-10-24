      * Suchkriterien und Trefferliste für SUCHE.
      * SU-BEGRIFF mit führendem * = Enthält-Suche, sonst Präfix.
      * SU-ARCHIV "J" = nur archivierte, sonst nur aktive Kontakte.
       01  SU-PARM.
           05  SU-BEGRIFF          PIC X(30).
           05  SU-ART              PIC X.
           05  SU-PLZ              PIC X(10).
           05  SU-ORT              PIC X(30).
           05  SU-KATEGORIE        PIC XX.
           05  SU-SCHLAGWORT       PIC X(11).
           05  SU-ARCHIV           PIC X.
           05  SU-FIRMA-NR         PIC X(7).
           05  SU-ANZAHL           PIC 9(4).
           05  SU-VOLL             PIC X.
           05  SU-TREFFER          OCCURS 500.
               10  ST-NR           PIC X(7).
               10  ST-ART          PIC X.
               10  ST-NAME         PIC X(40).
               10  ST-PLZ          PIC X(10).
               10  ST-ORT          PIC X(30).
               10  ST-KAT          PIC XX.
