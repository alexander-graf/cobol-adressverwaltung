      * Nach jedem ACCEPT: Shift+F1..F12 wie F1..F12, und ein Befehl
      * aus W-BEFEHL wird zum Tastencode aus W-BEF-TAB. Danach wertet
      * die Maske nur noch W-TASTE aus. Hyprland & Co. belegen die
      * nackten F-Tasten oft selbst, deshalb gibt es beide Wege.
       BEFEHL-AUSWERTEN.
           IF W-TASTE >= 1013 AND W-TASTE <= 1024
               SUBTRACT 12 FROM W-TASTE
           END-IF
           IF T-ENTER AND W-BEFEHL NOT = SPACES
               MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(W-BEFEHL))
                 TO W-BEFEHL
               PERFORM VARYING W-BEF-I FROM 1 BY 1
                       UNTIL W-BEF-WORT(W-BEF-I) = SPACES
                          OR W-BEF-WORT(W-BEF-I) = W-BEFEHL
                   CONTINUE
               END-PERFORM
               IF W-BEF-WORT(W-BEF-I) NOT = SPACES
                   MOVE W-BEF-CODE(W-BEF-I) TO W-TASTE
               ELSE
                   IF W-BEF-LISTE NOT = "J"
                       STRING "Unbekannter Befehl "
                              FUNCTION TRIM(W-BEFEHL)
                              " - ? zeigt alle Befehle."
                              DELIMITED BY SIZE INTO W-MELDUNG
                       MOVE 9999 TO W-TASTE
                   END-IF
               END-IF
           END-IF
           MOVE SPACES TO W-BEFEHL.
