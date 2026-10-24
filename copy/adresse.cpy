      * Adresssatz: beliebig viele je Kontakt, Schlüssel Nr + lfd. Nr.
      * AD-TYP: H Haupt/Geschäft, R Rechnung, L Lieferung,
      *         P Postfach, W privat
       01  AD-SATZ.
           05  AD-SCHLUESSEL.
               10  AD-KT-NR        PIC X(7).
               10  AD-LFD          PIC 99.
           05  AD-TYP              PIC X.
           05  AD-STANDARD         PIC X.
               88  AD-IST-STANDARD VALUE "J".
           05  AD-STRASSE          PIC X(50).
           05  AD-HAUSNR           PIC X(10).
           05  AD-ZUSATZ           PIC X(50).
           05  AD-PLZ              PIC X(10).
           05  AD-ORT              PIC X(40).
           05  AD-ORTSTEIL         PIC X(40).
           05  AD-REGION           PIC X(30).
           05  AD-LAND             PIC XX.
