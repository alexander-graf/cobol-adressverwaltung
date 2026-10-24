       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOIO.
      * Zugriff auf kommunikation.dat (Muster wie KTIO).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL KO-DATEI ASSIGN TO W-PFAD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DK-SCHLUESSEL
               FILE STATUS IS W-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  KO-DATEI.
       01  DK-SATZ.
           05  DK-SCHLUESSEL       PIC X(9).
           05  FILLER              PIC X(123).
       WORKING-STORAGE SECTION.
       01  W-PFAD                  PIC X(250).
       01  W-NAME                  PIC X(200) VALUE "kommunikation.dat".
       01  W-STATUS                PIC XX.
       01  W-OFFEN                 PIC X VALUE "N".
       LINKAGE SECTION.
       COPY ioparm.
       COPY komm.
       PROCEDURE DIVISION USING IO-PARM KO-SATZ.
           IF IO-FUNKTION = "SCHLIESS"
               IF W-OFFEN = "J"
                   CLOSE KO-DATEI
                   MOVE "N" TO W-OFFEN
               END-IF
               MOVE "00" TO IO-STATUS
               GOBACK
           END-IF
           IF W-OFFEN = "N"
               CALL "ADRVPFAD" USING W-NAME W-PFAD
               OPEN I-O KO-DATEI
               IF W-STATUS NOT = "00" AND NOT = "05"
                   MOVE W-STATUS TO IO-STATUS
                   GOBACK
               END-IF
               MOVE "J" TO W-OFFEN
           END-IF
           EVALUATE IO-FUNKTION
               WHEN "LESEN"
                   MOVE KO-SCHLUESSEL TO DK-SCHLUESSEL
                   READ KO-DATEI INTO KO-SATZ KEY IS DK-SCHLUESSEL
               WHEN "SCHREIB"
                   WRITE DK-SATZ FROM KO-SATZ
               WHEN "AENDERN"
                   REWRITE DK-SATZ FROM KO-SATZ
               WHEN "LOESCHEN"
                   MOVE KO-SCHLUESSEL TO DK-SCHLUESSEL
                   DELETE KO-DATEI
               WHEN "START-NR"
                   MOVE KO-SCHLUESSEL TO DK-SCHLUESSEL
                   START KO-DATEI KEY IS >= DK-SCHLUESSEL
               WHEN "WEITER"
                   READ KO-DATEI NEXT RECORD INTO KO-SATZ
               WHEN OTHER
                   MOVE "99" TO W-STATUS
           END-EVALUATE
           MOVE W-STATUS TO IO-STATUS
           GOBACK.
