      * Kopfzeile, zwei Tastenzeilen und Meldungszeile jeder Maske.
      * BLANK SCREEN wirkt in GnuCOBOL 3.2 nur als eigenes Element,
      * nicht als Klausel auf Stufe 01.
           05  BLANK SCREEN.
           05  LINE 1 COLUMN 1 PIC X(80) FROM W-KOPF REVERSE-VIDEO.
           05  LINE 22 COLUMN 1 PIC X(80) FROM W-TASTEN1
               FOREGROUND-COLOR 6.
           05  LINE 23 COLUMN 1 PIC X(80) FROM W-TASTEN2
               FOREGROUND-COLOR 6.
           05  LINE 24 COLUMN 1 PIC X(79) FROM W-MELDUNG
               FOREGROUND-COLOR 3 HIGHLIGHT.
