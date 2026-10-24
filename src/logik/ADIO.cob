       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADIO.
      * Zugriff auf die Adressdatei adressen.dat (Muster wie KTIO).
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL AD-DATEI ASSIGN TO W-PFAD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DA-SCHLUESSEL
               FILE STATUS IS W-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  AD-DATEI.
       01  DA-SATZ.
           05  DA-SCHLUESSEL       PIC X(9).
           05  FILLER              PIC X(234).
       WORKING-STORAGE SECTION.
       01  W-PFAD                  PIC X(250).
       01  W-NAME                  PIC X(200) VALUE "adressen.dat".
       01  W-STATUS                PIC XX.
       01  W-OFFEN                 PIC X VALUE "N".
       LINKAGE SECTION.
       COPY ioparm.
       COPY adresse.
       PROCEDURE DIVISION USING IO-PARM AD-SATZ.
           IF IO-FUNKTION = "SCHLIESS"
               IF W-OFFEN = "J"
                   CLOSE AD-DATEI
                   MOVE "N" TO W-OFFEN
               END-IF
               MOVE "00" TO IO-STATUS
               GOBACK
           END-IF
           IF W-OFFEN = "N"
               CALL "ADRVPFAD" USING W-NAME W-PFAD
               OPEN I-O AD-DATEI
               IF W-STATUS NOT = "00" AND NOT = "05"
                   MOVE W-STATUS TO IO-STATUS
                   GOBACK
               END-IF
               MOVE "J" TO W-OFFEN
           END-IF
           EVALUATE IO-FUNKTION
               WHEN "LESEN"
                   MOVE AD-SCHLUESSEL TO DA-SCHLUESSEL
                   READ AD-DATEI INTO AD-SATZ KEY IS DA-SCHLUESSEL
               WHEN "SCHREIB"
                   WRITE DA-SATZ FROM AD-SATZ
               WHEN "AENDERN"
                   REWRITE DA-SATZ FROM AD-SATZ
               WHEN "LOESCHEN"
                   MOVE AD-SCHLUESSEL TO DA-SCHLUESSEL
                   DELETE AD-DATEI
               WHEN "START-NR"
                   MOVE AD-SCHLUESSEL TO DA-SCHLUESSEL
                   START AD-DATEI KEY IS >= DA-SCHLUESSEL
               WHEN "WEITER"
                   READ AD-DATEI NEXT RECORD INTO AD-SATZ
               WHEN OTHER
                   MOVE "99" TO W-STATUS
           END-EVALUATE
           MOVE W-STATUS TO IO-STATUS
           GOBACK.
