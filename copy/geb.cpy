      * Geburtstagsliste (Modul GEBLISTE).
       01  GL-PARM.
           05  GL-HEUTE            PIC 9(8).
           05  GL-TAGE             PIC 999.
           05  GL-ANZAHL           PIC 999.
           05  GL-EINTRAG          OCCURS 200.
               10  GL-NR           PIC X(7).
               10  GL-NAME         PIC X(50).
               10  GL-GEBDATUM     PIC 9(8).
               10  GL-IN-TAGEN     PIC 999.
               10  GL-WIRD         PIC 999.
