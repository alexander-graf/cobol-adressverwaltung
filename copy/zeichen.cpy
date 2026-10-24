      * Umwandlung Latin-9 <-> UTF-8 (Modul ZEICHEN).
      * ZC-RICHTUNG "U" = nach UTF-8, "L" = nach Latin-9.
       01  ZC-PARM.
           05  ZC-RICHTUNG         PIC X.
           05  ZC-EIN-LEN          PIC 9(5).
           05  ZC-EIN              PIC X(8000).
           05  ZC-AUS-LEN          PIC 9(5).
           05  ZC-AUS              PIC X(16000).
           05  ZC-ERSETZT          PIC 9(5).
