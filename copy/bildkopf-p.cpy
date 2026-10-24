      * Kopfzeile: links " Adressen · Titel", rechts
      * W-KOPF-RECHTS (leer = heutiges Datum).
       KOPF-BAUEN.
           MOVE SPACES TO W-KOPF
           STRING " Adressen · " DELIMITED BY SIZE
                  FUNCTION TRIM(W-TITEL) DELIMITED BY SIZE
             INTO W-KOPF
           IF W-KOPF-RECHTS = SPACES
               STRING FUNCTION CURRENT-DATE(7:2) "."
                      FUNCTION CURRENT-DATE(5:2) "."
                      FUNCTION CURRENT-DATE(1:4)
                      DELIMITED BY SIZE INTO W-KOPF-RECHTS
           END-IF
           MOVE FUNCTION LENGTH(FUNCTION TRIM(W-KOPF-RECHTS))
             TO W-KOPF-LEN
           MOVE FUNCTION TRIM(W-KOPF-RECHTS)
             TO W-KOPF(80 - W-KOPF-LEN:W-KOPF-LEN).
