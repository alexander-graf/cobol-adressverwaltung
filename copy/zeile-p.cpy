      * Zeilenaufbau in Latin-9 und Ausgabe als UTF-8-Bytestrom.
      * Das einbindende Programm braucht COPY zeichen. und
      * COPY zeile-d. im WORKING-STORAGE.
      *
      * AUSGABE-OEFFNEN legt W-AUS-PFAD neu an (W-AUS-RC = 0: gut).
       AUSGABE-OEFFNEN.
           MOVE "N" TO W-CRLF
           MOVE 0 TO W-AUS-OFFSET
           MOVE W-AUS-PFAD TO W-FELD
           MOVE SPACES TO W-AUS-PFAD
           STRING FUNCTION TRIM(W-FELD) X"00" DELIMITED BY SIZE
             INTO W-AUS-PFAD
           CALL "CBL_CREATE_FILE" USING W-AUS-PFAD W-AUS-ZUGRIFF
                W-AUS-SPERRE W-AUS-GERAET W-AUS-HANDLE
           MOVE RETURN-CODE TO W-AUS-RC
           MOVE 0 TO RETURN-CODE
           PERFORM ZEILE-LEEREN.

       AUSGABE-SCHLIESSEN.
           CALL "CBL_CLOSE_FILE" USING W-AUS-HANDLE
           MOVE 0 TO RETURN-CODE.

      * ZC-AUS(1:W-AUS-ANZ) unverändert in die Datei schreiben.
       BYTES-SCHREIBEN.
           IF W-AUS-ANZ > 0
               CALL "CBL_WRITE_FILE" USING W-AUS-HANDLE W-AUS-OFFSET
                    W-AUS-ANZ W-AUS-FLAGS ZC-AUS
               ADD W-AUS-ANZ TO W-AUS-OFFSET
               MOVE 0 TO RETURN-CODE
           END-IF.

       ZEILE-LEEREN.
           MOVE SPACES TO W-ZEILE
           MOVE 0 TO W-ZPOS.

      * W-FELD(1:W-FLEN) unverändert anhängen.
       ROH-ANHAENGEN.
           IF W-FLEN > 0 AND W-ZPOS + W-FLEN <= LENGTH OF W-ZEILE
               MOVE W-FELD(1:W-FLEN) TO W-ZEILE(W-ZPOS + 1:W-FLEN)
               ADD W-FLEN TO W-ZPOS
           END-IF.

      * Literal ohne Leerzeichen am Ende anhängen: MOVE ... TO W-FELD.
       TEXT-ANHAENGEN.
           PERFORM FELD-LAENGE
           PERFORM ROH-ANHAENGEN.

       FELD-LAENGE.
           IF W-FELD = SPACES
               MOVE 0 TO W-FLEN
           ELSE
               MOVE FUNCTION LENGTH(FUNCTION TRIM(W-FELD TRAILING))
                 TO W-FLEN
           END-IF.

      * W-FELD ohne Leerzeichen am Ende, maskiert nach W-MODUS.
       WERT-ANHAENGEN.
           PERFORM FELD-LAENGE
           PERFORM VARYING W-FK FROM 1 BY 1 UNTIL W-FK > W-FLEN
               MOVE W-FELD(W-FK:1) TO W-FZ
               IF W-ZPOS < LENGTH OF W-ZEILE - 1
                   EVALUATE TRUE
                       WHEN W-MODUS = "C" AND W-FZ = QUOTE
                           ADD 1 TO W-ZPOS
                           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
                       WHEN W-MODUS = "J"
                        AND (W-FZ = QUOTE OR W-FZ = "\")
                           ADD 1 TO W-ZPOS
                           MOVE "\" TO W-ZEILE(W-ZPOS:1)
                       WHEN W-MODUS = "V"
                        AND (W-FZ = "," OR W-FZ = ";" OR W-FZ = "\")
                           ADD 1 TO W-ZPOS
                           MOVE "\" TO W-ZEILE(W-ZPOS:1)
                   END-EVALUATE
                   ADD 1 TO W-ZPOS
                   MOVE W-FZ TO W-ZEILE(W-ZPOS:1)
               END-IF
           END-PERFORM.

      * Zeile nach UTF-8 wandeln und mit LF (vCard: CRLF) schreiben.
       ZEILE-AUSGEBEN.
           MOVE "U" TO ZC-RICHTUNG
           MOVE W-ZPOS TO ZC-EIN-LEN
           MOVE W-ZEILE TO ZC-EIN
           CALL "ZEICHEN" USING ZC-PARM
           IF W-CRLF = "J"
               ADD 1 TO ZC-AUS-LEN
               MOVE X"0D" TO ZC-AUS(ZC-AUS-LEN:1)
           END-IF
           ADD 1 TO ZC-AUS-LEN
           MOVE X"0A" TO ZC-AUS(ZC-AUS-LEN:1)
           MOVE ZC-AUS-LEN TO W-AUS-ANZ
           PERFORM BYTES-SCHREIBEN
           PERFORM ZEILE-LEEREN.
