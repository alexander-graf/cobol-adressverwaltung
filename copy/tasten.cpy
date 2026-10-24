      * Gemeinsame Felder aller Masken. Jedes Maskenprogramm setzt
      * in SPECIAL-NAMES: CRT STATUS IS W-TASTE, CURSOR IS W-CURSOR.
       01  W-TASTE                 PIC 9(4).
           88  T-ENTER             VALUE 0.
           88  T-F1                VALUE 1001.
           88  T-F2                VALUE 1002.
           88  T-F3                VALUE 1003 2005.
           88  T-F4                VALUE 1004.
           88  T-F5                VALUE 1005.
           88  T-F6                VALUE 1006.
           88  T-F7                VALUE 1007.
           88  T-F8                VALUE 1008.
           88  T-F9                VALUE 1009.
           88  T-F10               VALUE 1010.
           88  T-F11               VALUE 1011.
           88  T-F12               VALUE 1012.
           88  T-SEITE-ZURUECK     VALUE 2001.
           88  T-SEITE-VOR         VALUE 2002.
       01  W-CURSOR                PIC 9(4).
       01  W-CUR REDEFINES W-CURSOR.
           05  W-CUR-ZEILE         PIC 99.
           05  W-CUR-SPALTE        PIC 99.
       01  W-TITEL                 PIC X(50).
       01  W-KOPF-RECHTS           PIC X(40).
       01  W-KOPF                  PIC X(80).
       01  W-TASTEN1               PIC X(80).
       01  W-TASTEN2               PIC X(80).
       01  W-MELDUNG               PIC X(79).
       01  W-KOPF-LEN              PIC 99.
       01  W-BESTAETIGEN           PIC X(8).
