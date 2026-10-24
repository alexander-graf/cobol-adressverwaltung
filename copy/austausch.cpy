      * Export, Import und Etiketten.
      * AU-FORMAT: C = CSV, V = vCard, J = JSON (nur Export)
       01  AU-PARM.
           05  AU-FORMAT           PIC X.
           05  AU-DATEI            PIC X(200).
           05  AU-PROTOKOLL        PIC X(200).
           05  AU-MIT-ARCHIV       PIC X.
           05  AU-MIT-ANREDE       PIC X.
           05  AU-ANZAHL           PIC 9(5).
           05  AU-UEBERSPRUNGEN    PIC 9(5).
           05  AU-ERSETZT          PIC 9(5).
           05  AU-OK               PIC X.
               88  AU-ERFOLG       VALUE "J".
           05  AU-MELDUNG          PIC X(70).
