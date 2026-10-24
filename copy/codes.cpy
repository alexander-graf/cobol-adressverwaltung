      * Wertelisten (Modul CODES).
      * CD-LISTE: KATEG RECHTSF ANREDE ADRTYP LAND KOMMTYP
      * CD-FUNKTION: LISTE (füllt Tabelle), TEXT, PRUEF
       01  CD-PARM.
           05  CD-FUNKTION         PIC X(8).
           05  CD-LISTE            PIC X(8).
           05  CD-CODE             PIC X(20).
           05  CD-TEXT             PIC X(30).
           05  CD-OK               PIC X.
               88  CD-GUELTIG      VALUE "J".
           05  CD-ANZAHL           PIC 99.
           05  CD-EINTRAG          OCCURS 20.
               10  CD-E-CODE       PIC X(20).
               10  CD-E-TEXT       PIC X(30).
