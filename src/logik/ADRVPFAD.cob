       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADRVPFAD.
      * Macht aus einem Dateinamen einen vollen Pfad:
      *   "/..."  bleibt wie er ist
      *   "~/..." liegt im Heimatverzeichnis
      *   sonst   liegt im Datenverzeichnis ($ADRV_DATEN, ersatzweise
      *           ~/.local/share/adressverwaltung)
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-VERZ                  PIC X(200).
       01  W-HOME                  PIC X(200).
       LINKAGE SECTION.
       01  PF-DATEI                PIC X(200).
       01  PF-PFAD                 PIC X(250).
       PROCEDURE DIVISION USING PF-DATEI PF-PFAD.
           MOVE SPACES TO PF-PFAD W-VERZ W-HOME
           ACCEPT W-HOME FROM ENVIRONMENT "HOME"
           EVALUATE TRUE
               WHEN PF-DATEI(1:1) = "/"
                   MOVE PF-DATEI TO PF-PFAD
               WHEN PF-DATEI(1:2) = "~/"
                   STRING FUNCTION TRIM(W-HOME) DELIMITED BY SIZE
                          FUNCTION TRIM(PF-DATEI(2:)) DELIMITED BY SIZE
                     INTO PF-PFAD
               WHEN OTHER
                   ACCEPT W-VERZ FROM ENVIRONMENT "ADRV_DATEN"
                   IF W-VERZ = SPACES
                       STRING FUNCTION TRIM(W-HOME) DELIMITED BY SIZE
                              "/.local/share/adressverwaltung"
                              DELIMITED BY SIZE
                         INTO W-VERZ
                   END-IF
                   STRING FUNCTION TRIM(W-VERZ) DELIMITED BY SIZE
                          "/" DELIMITED BY SIZE
                          FUNCTION TRIM(PF-DATEI) DELIMITED BY SIZE
                     INTO PF-PFAD
           END-EVALUATE
           GOBACK.
