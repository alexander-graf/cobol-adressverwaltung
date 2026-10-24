      * Prüfungen (Modul PRUEF).
      * PR-FUNKTION: PLZ EMAIL USTID LAND DATUM GEBDAT DATTEXT
      * DATUM/GEBDAT: PR-WERT "TT.MM.JJJJ" -> PR-DATUM (JJJJMMTT)
      * DATTEXT:      PR-DATUM -> PR-WERT
       01  PR-PARM.
           05  PR-FUNKTION         PIC X(8).
           05  PR-WERT             PIC X(100).
           05  PR-LAND             PIC XX.
           05  PR-DATUM            PIC 9(8).
           05  PR-OK               PIC X.
               88  PR-GUELTIG      VALUE "J".
           05  PR-MELDUNG          PIC X(60).
