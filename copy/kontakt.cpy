      * Kontaktsatz: Firma (KT-ART "F") oder Person (KT-ART "P").
      * Der Datenteil KT-DATEN wird je nach Art umdefiniert.
       01  KT-SATZ.
           05  KT-NR               PIC X(7).
           05  KT-ART              PIC X.
               88  KT-IST-FIRMA    VALUE "F".
               88  KT-IST-PERSON   VALUE "P".
           05  KT-SUCH             PIC X(40).
           05  KT-FIRMA-NR         PIC X(7).
           05  KT-KATEGORIE        PIC XX.
           05  KT-SCHLAGWORT       PIC X(11) OCCURS 5.
           05  KT-ARCHIV           PIC X.
               88  KT-ARCHIVIERT   VALUE "J".
           05  KT-ANGELEGT         PIC 9(14).
           05  KT-GEAENDERT        PIC 9(14).
           05  KT-NOTIZ            PIC X(60) OCCURS 4.
           05  KT-DATEN            PIC X(400).
           05  KT-FIRMA REDEFINES KT-DATEN.
               10  KF-NAME         PIC X(60).
               10  KF-ZUSATZ       PIC X(60).
               10  KF-RECHTSFORM   PIC X(20).
               10  KF-BRANCHE      PIC X(30).
               10  KF-WEBSITE      PIC X(60).
               10  KF-USTID        PIC X(15).
               10  KF-STEUERNR     PIC X(20).
               10  KF-REGGERICHT   PIC X(30).
               10  KF-HRNR         PIC X(20).
               10  KF-REFERENZ     PIC X(20).
               10  FILLER          PIC X(65).
           05  KT-PERSON REDEFINES KT-DATEN.
               10  KP-ANREDE       PIC X.
               10  KP-TITEL        PIC X(20).
               10  KP-VORNAME      PIC X(30).
               10  KP-VORNAMEN2    PIC X(30).
               10  KP-VORSATZ      PIC X(20).
               10  KP-NACHNAME     PIC X(40).
               10  KP-ZUSATZ       PIC X(10).
               10  KP-GEBDATUM     PIC 9(8).
               10  KP-POSITION     PIC X(40).
               10  KP-ABTEILUNG    PIC X(40).
               10  FILLER          PIC X(161).
