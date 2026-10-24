       IDENTIFICATION DIVISION.
       PROGRAM-ID. KTIO.
      * Zugriff auf die Kontaktdatei kontakte.dat.
      * Die Datei wird beim ersten Aufruf geöffnet (und angelegt,
      * falls sie fehlt) und bleibt offen bis SCHLIESS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL KT-DATEI ASSIGN TO W-PFAD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS KD-NR
               ALTERNATE RECORD KEY IS KD-SUCH WITH DUPLICATES
               ALTERNATE RECORD KEY IS KD-FIRMA-NR WITH DUPLICATES
               FILE STATUS IS W-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  KT-DATEI.
       01  KD-SATZ.
           05  KD-NR               PIC X(7).
           05  FILLER              PIC X.
           05  KD-SUCH             PIC X(40).
           05  KD-FIRMA-NR         PIC X(7).
           05  FILLER              PIC X(726).
       WORKING-STORAGE SECTION.
       01  W-PFAD                  PIC X(250).
       01  W-NAME                  PIC X(200) VALUE "kontakte.dat".
       01  W-STATUS                PIC XX.
       01  W-OFFEN                 PIC X VALUE "N".
       LINKAGE SECTION.
       COPY ioparm.
       COPY kontakt.
       PROCEDURE DIVISION USING IO-PARM KT-SATZ.
           IF IO-FUNKTION = "SCHLIESS"
               IF W-OFFEN = "J"
                   CLOSE KT-DATEI
                   MOVE "N" TO W-OFFEN
               END-IF
               MOVE "00" TO IO-STATUS
               GOBACK
           END-IF
           IF W-OFFEN = "N"
               CALL "ADRVPFAD" USING W-NAME W-PFAD
               OPEN I-O KT-DATEI
               IF W-STATUS NOT = "00" AND NOT = "05"
                   MOVE W-STATUS TO IO-STATUS
                   GOBACK
               END-IF
               MOVE "J" TO W-OFFEN
           END-IF
           EVALUATE IO-FUNKTION
               WHEN "LESEN"
                   MOVE KT-NR TO KD-NR
                   READ KT-DATEI INTO KT-SATZ KEY IS KD-NR
               WHEN "SCHREIB"
                   WRITE KD-SATZ FROM KT-SATZ
               WHEN "AENDERN"
                   REWRITE KD-SATZ FROM KT-SATZ
               WHEN "LOESCHEN"
                   MOVE KT-NR TO KD-NR
                   DELETE KT-DATEI
               WHEN "START-NR"
                   MOVE KT-NR TO KD-NR
                   START KT-DATEI KEY IS >= KD-NR
               WHEN "START-SU"
                   MOVE KT-SUCH TO KD-SUCH
                   START KT-DATEI KEY IS >= KD-SUCH
               WHEN "START-FI"
                   MOVE KT-FIRMA-NR TO KD-FIRMA-NR
                   START KT-DATEI KEY IS = KD-FIRMA-NR
               WHEN "WEITER"
                   READ KT-DATEI NEXT RECORD INTO KT-SATZ
               WHEN OTHER
                   MOVE "99" TO W-STATUS
           END-EVALUATE
           MOVE W-STATUS TO IO-STATUS
           GOBACK.
