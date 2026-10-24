       IDENTIFICATION DIVISION.
       PROGRAM-ID. NRVERG.
      * Vergibt die nächste Kontaktnummer je Art (F oder P) aus
      * zaehler.dat. Nummern werden nie wiederverwendet.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL ZA-DATEI ASSIGN TO W-PFAD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ZA-ART
               FILE STATUS IS W-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ZA-DATEI.
       01  ZA-SATZ.
           05  ZA-ART              PIC X.
           05  ZA-WERT             PIC 9(6).
       WORKING-STORAGE SECTION.
       01  W-PFAD                  PIC X(250).
       01  W-NAME                  PIC X(200) VALUE "zaehler.dat".
       01  W-STATUS                PIC XX.
       LINKAGE SECTION.
       01  NV-ART                  PIC X.
       01  NV-NR                   PIC X(7).
       01  NV-STATUS               PIC XX.
       PROCEDURE DIVISION USING NV-ART NV-NR NV-STATUS.
           MOVE SPACES TO NV-NR
           CALL "ADRVPFAD" USING W-NAME W-PFAD
           OPEN I-O ZA-DATEI
           IF W-STATUS NOT = "00" AND NOT = "05"
               MOVE W-STATUS TO NV-STATUS
               GOBACK
           END-IF
           MOVE NV-ART TO ZA-ART
           READ ZA-DATEI
           IF W-STATUS = "00"
               ADD 1 TO ZA-WERT
               REWRITE ZA-SATZ
           ELSE
               MOVE NV-ART TO ZA-ART
               MOVE 1 TO ZA-WERT
               WRITE ZA-SATZ
           END-IF
           MOVE W-STATUS TO NV-STATUS
           IF W-STATUS = "00"
               STRING NV-ART ZA-WERT DELIMITED BY SIZE INTO NV-NR
           END-IF
           CLOSE ZA-DATEI
           GOBACK.
