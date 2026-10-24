      * Kopfzeile rechts: angelegt/geändert eines Kontakts, und
      * bevorzugte E-Mail und Telefon als Text. Braucht KT-SATZ,
      * DI-PARM, AD-SATZ, KO-SATZ und W-KOMM-TEXT.
       KOPF-ZEITEN.
           MOVE SPACES TO W-KOPF-RECHTS
           STRING "angelegt " KT-ANGELEGT(7:2) "." KT-ANGELEGT(5:2)
                  "." KT-ANGELEGT(3:2) "  geändert "
                  KT-GEAENDERT(7:2) "." KT-GEAENDERT(5:2) "."
                  KT-GEAENDERT(3:2) DELIMITED BY SIZE
             INTO W-KOPF-RECHTS.

       KOMM-TEXT-BILDEN.
           MOVE SPACES TO W-KOMM-TEXT
           MOVE "EM" TO DI-KOMMTYP
           MOVE "BEVKOMM" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF DI-ERFOLG
               MOVE KO-WERT TO W-KOMM-TEXT
           END-IF
           MOVE "TE" TO DI-KOMMTYP
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF NOT DI-ERFOLG
               MOVE "MO" TO DI-KOMMTYP
               CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           END-IF
           IF DI-ERFOLG
               IF W-KOMM-TEXT = SPACES
                   MOVE KO-WERT TO W-KOMM-TEXT
               ELSE
                   STRING FUNCTION TRIM(W-KOMM-TEXT) "  |  "
                          FUNCTION TRIM(KO-WERT) DELIMITED BY SIZE
                     INTO W-KOMM-TEXT
               END-IF
           END-IF
           IF W-KOMM-TEXT = SPACES
               MOVE "(keine - F10)" TO W-KOMM-TEXT
           END-IF.

       KURZNAME-BILDEN.
           MOVE SPACES TO W-KURZNAME
           IF KT-IST-FIRMA
               MOVE KF-NAME TO W-KURZNAME
           ELSE
               STRING FUNCTION TRIM(KP-VORNAME) " "
                      FUNCTION TRIM(KP-NACHNAME) DELIMITED BY SIZE
                 INTO W-KURZNAME
           END-IF.
