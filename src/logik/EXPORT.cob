       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPORT.
      * Exportiert alle Kontakte (ohne archivierte, außer
      * AU-MIT-ARCHIV = "J") als UTF-8-Datei:
      *   C  CSV mit Semikolon und BOM, eine Zeile je Kontakt mit
      *      Standardadresse und bevorzugter E-Mail/Telefon/Mobil.
      *      Dieses Format liest IMPORT wieder ein.
      *   V  vCard 3.0 mit allen Adressen und Kommunikationswegen
      *   J  JSON-Feld mit allen Adressen und Kommunikationswegen
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY zeichen.
       COPY codes.
       COPY pruef.
       COPY zeile-d.
       COPY wort-d.
       01  W-I                     PIC 9(4).
       01  W-J                     PIC 99.
       01  W-ERSTES                PIC X.
       01  W-JSCHLUESSEL           PIC X(30).
       01  W-FI-ANZ                PIC 9(4).
       01  W-FI                    OCCURS 3000.
           05  W-FI-NR             PIC X(7).
           05  W-FI-NAME           PIC X(60).
       01  W-FI-FIRMA              PIC X(60).
       01  W-ADR-ANZ               PIC 99.
       01  W-ADR                   PIC X(243) OCCURS 99.
       01  W-KOM-ANZ               PIC 99.
       01  W-KOM                   PIC X(132) OCCURS 99.
       01  W-ZEIT                  PIC X(14).
       01  W-NR-FUER-LADEN         PIC X(7).
       01  W-ISO                   PIC X(19).
       01  W-DATUM                 PIC 9(8).
       01  W-DATUM-X REDEFINES W-DATUM PIC X(8).
       LINKAGE SECTION.
       COPY austausch.
       PROCEDURE DIVISION USING AU-PARM.
           MOVE 0 TO AU-ANZAHL
           MOVE "J" TO AU-OK
           MOVE SPACES TO AU-MELDUNG
           CALL "ADRVPFAD" USING AU-DATEI W-AUS-PFAD
           PERFORM AUSGABE-OEFFNEN
           IF W-AUS-RC NOT = 0
               MOVE "N" TO AU-OK
               MOVE "Datei lässt sich nicht anlegen" TO AU-MELDUNG
               GOBACK
           END-IF
           PERFORM FIRMEN-MERKEN
           EVALUATE AU-FORMAT
               WHEN "C"
                   MOVE "C" TO W-MODUS
                   PERFORM CSV-KOPF
               WHEN "V"
                   MOVE "V" TO W-MODUS
                   MOVE "J" TO W-CRLF
               WHEN OTHER
                   MOVE "J" TO W-MODUS
                   MOVE "[" TO W-FELD
                   PERFORM TEXT-ANHAENGEN
                   PERFORM ZEILE-AUSGEBEN
           END-EVALUATE
           MOVE LOW-VALUES TO KT-NR
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM UNTIL NOT IO-OK
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK
                  AND (NOT KT-ARCHIVIERT OR AU-MIT-ARCHIV = "J")
                   EVALUATE AU-FORMAT
                       WHEN "C" PERFORM CSV-KONTAKT
                       WHEN "V" PERFORM VCARD-KONTAKT
                       WHEN OTHER PERFORM JSON-KONTAKT
                   END-EVALUATE
                   ADD 1 TO AU-ANZAHL
               END-IF
           END-PERFORM
           IF AU-FORMAT = "J"
               MOVE "]" TO W-FELD
               PERFORM TEXT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-IF
           PERFORM AUSGABE-SCHLIESSEN
           MOVE AU-ANZAHL TO W-ZAHL
           STRING FUNCTION TRIM(W-ZAHL) " Kontakt(e) exportiert"
                  DELIMITED BY SIZE INTO AU-MELDUNG
           GOBACK.

      * Firmennamen vorab merken; ein Lesen mitten im Durchlauf
      * würde die Leseposition in kontakte.dat verschieben.
       FIRMEN-MERKEN.
           MOVE 0 TO W-FI-ANZ
           MOVE "F" TO KT-NR
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           PERFORM UNTIL NOT IO-OK OR W-FI-ANZ >= 3000
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KTIO" USING IO-PARM KT-SATZ
               IF IO-OK AND KT-IST-FIRMA
                   ADD 1 TO W-FI-ANZ
                   MOVE KT-NR TO W-FI-NR(W-FI-ANZ)
                   MOVE KF-NAME TO W-FI-NAME(W-FI-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM.

       FIRMENNAME-SUCHEN.
           MOVE SPACES TO W-FI-FIRMA
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-FI-ANZ
               IF W-FI-NR(W-I) = KT-FIRMA-NR
                   MOVE W-FI-NAME(W-I) TO W-FI-FIRMA
                   MOVE W-FI-ANZ TO W-I
               END-IF
           END-PERFORM.

      ******************************************************************
      * CSV
      ******************************************************************
       CSV-KOPF.
           MOVE X"EFBBBF" TO ZC-AUS
           MOVE 3 TO W-AUS-ANZ
           PERFORM BYTES-SCHREIBEN
           MOVE "Nr;Art;Kategorie;Schlagwörter;Firmenname;Firmenzusatz;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Rechtsform;Branche;Website;USt-IdNr;Steuernummer;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Registergericht;HR-Nr;Referenz;Anrede;Titel;Vorname;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Weitere Vornamen;Namensvorsatz;Nachname;Namenszusatz;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Geburtsdatum;Firma-Nr;Position;Abteilung;Adresstyp;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Straße;Hausnr;Adresszusatz;PLZ;Ort;Ortsteil;Region;"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE "Land;E-Mail;Telefon;Mobil;Notiz;Archiviert"
             TO W-FELD
           PERFORM TEXT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN.

       CSV-KONTAKT.
           MOVE "J" TO W-ERSTES
           MOVE KT-NR TO W-FELD           PERFORM CSV-FELD
           MOVE KT-ART TO W-FELD          PERFORM CSV-FELD
           MOVE KT-KATEGORIE TO W-FELD    PERFORM CSV-FELD
           PERFORM SCHLAGWORTE-ALS-TEXT   PERFORM CSV-FELD
           IF KT-IST-FIRMA
               MOVE KF-NAME TO W-FELD         PERFORM CSV-FELD
               MOVE KF-ZUSATZ TO W-FELD       PERFORM CSV-FELD
               MOVE KF-RECHTSFORM TO W-FELD   PERFORM CSV-FELD
               MOVE KF-BRANCHE TO W-FELD      PERFORM CSV-FELD
               MOVE KF-WEBSITE TO W-FELD      PERFORM CSV-FELD
               MOVE KF-USTID TO W-FELD        PERFORM CSV-FELD
               MOVE KF-STEUERNR TO W-FELD     PERFORM CSV-FELD
               MOVE KF-REGGERICHT TO W-FELD   PERFORM CSV-FELD
               MOVE KF-HRNR TO W-FELD         PERFORM CSV-FELD
               MOVE KF-REFERENZ TO W-FELD     PERFORM CSV-FELD
               MOVE SPACES TO W-FELD
               PERFORM CSV-FELD 11 TIMES
           ELSE
               MOVE SPACES TO W-FELD
               PERFORM CSV-FELD 10 TIMES
               MOVE KP-ANREDE TO W-FELD       PERFORM CSV-FELD
               MOVE KP-TITEL TO W-FELD        PERFORM CSV-FELD
               MOVE KP-VORNAME TO W-FELD      PERFORM CSV-FELD
               MOVE KP-VORNAMEN2 TO W-FELD    PERFORM CSV-FELD
               MOVE KP-VORSATZ TO W-FELD      PERFORM CSV-FELD
               MOVE KP-NACHNAME TO W-FELD     PERFORM CSV-FELD
               MOVE KP-ZUSATZ TO W-FELD       PERFORM CSV-FELD
               MOVE "DATTEXT" TO PR-FUNKTION
               MOVE KP-GEBDATUM TO PR-DATUM
               CALL "PRUEF" USING PR-PARM
               MOVE PR-WERT TO W-FELD         PERFORM CSV-FELD
               MOVE KT-FIRMA-NR TO W-FELD     PERFORM CSV-FELD
               MOVE KP-POSITION TO W-FELD     PERFORM CSV-FELD
               MOVE KP-ABTEILUNG TO W-FELD    PERFORM CSV-FELD
           END-IF
           MOVE "STDADR" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF NOT DI-ERFOLG
               MOVE SPACES TO AD-SATZ
           END-IF
           MOVE AD-TYP TO W-FELD          PERFORM CSV-FELD
           MOVE AD-STRASSE TO W-FELD      PERFORM CSV-FELD
           MOVE AD-HAUSNR TO W-FELD       PERFORM CSV-FELD
           MOVE AD-ZUSATZ TO W-FELD       PERFORM CSV-FELD
           MOVE AD-PLZ TO W-FELD          PERFORM CSV-FELD
           MOVE AD-ORT TO W-FELD          PERFORM CSV-FELD
           MOVE AD-ORTSTEIL TO W-FELD     PERFORM CSV-FELD
           MOVE AD-REGION TO W-FELD       PERFORM CSV-FELD
           MOVE AD-LAND TO W-FELD         PERFORM CSV-FELD
           MOVE "EM" TO DI-KOMMTYP        PERFORM CSV-KOMM
           MOVE "TE" TO DI-KOMMTYP        PERFORM CSV-KOMM
           MOVE "MO" TO DI-KOMMTYP        PERFORM CSV-KOMM
           PERFORM NOTIZ-ALS-TEXT
           PERFORM CSV-FELD
           MOVE KT-ARCHIV TO W-FELD       PERFORM CSV-FELD
           PERFORM ZEILE-AUSGEBEN.

       CSV-KOMM.
           MOVE "BEVKOMM" TO DI-FUNKTION
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           MOVE SPACES TO W-FELD
           IF DI-ERFOLG
               MOVE KO-WERT TO W-FELD
           END-IF
           PERFORM CSV-FELD.

       CSV-FELD.
           IF W-ERSTES = "N"
               ADD 1 TO W-ZPOS
               MOVE ";" TO W-ZEILE(W-ZPOS:1)
           END-IF
           MOVE "N" TO W-ERSTES
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           PERFORM WERT-ANHAENGEN
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1).

      * Schlagwörter mit Komma, Notizzeilen mit " | " verbunden.
       SCHLAGWORTE-ALS-TEXT.
           MOVE SPACES TO W-SATZ
           MOVE 0 TO W-SATZ-POS
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 5
               IF KT-SCHLAGWORT(W-J) NOT = SPACES
                   IF W-SATZ-POS > 0
                       ADD 1 TO W-SATZ-POS
                       MOVE "," TO W-SATZ(W-SATZ-POS:1)
                   END-IF
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(
                        KT-SCHLAGWORT(W-J))) TO W-WORT-LEN
                   MOVE FUNCTION TRIM(KT-SCHLAGWORT(W-J))
                     TO W-SATZ(W-SATZ-POS + 1:W-WORT-LEN)
                   ADD W-WORT-LEN TO W-SATZ-POS
               END-IF
           END-PERFORM
           MOVE W-SATZ TO W-FELD.

       NOTIZ-ALS-TEXT.
           MOVE SPACES TO W-SATZ W-FELD
           MOVE 0 TO W-SATZ-POS
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 4
               IF KT-NOTIZ(W-J) NOT = SPACES
                   IF W-SATZ-POS > 0
                       MOVE " | " TO W-FELD(W-SATZ-POS + 1:3)
                       ADD 3 TO W-SATZ-POS
                   END-IF
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(
                        KT-NOTIZ(W-J) TRAILING)) TO W-WORT-LEN
                   MOVE KT-NOTIZ(W-J)(1:W-WORT-LEN)
                     TO W-FELD(W-SATZ-POS + 1:W-WORT-LEN)
                   ADD W-WORT-LEN TO W-SATZ-POS
               END-IF
           END-PERFORM.

      ******************************************************************
      * vCard 3.0
      ******************************************************************
       VCARD-KONTAKT.
           MOVE "BEGIN:VCARD" TO W-FELD   PERFORM VC-ZEILE
           MOVE "VERSION:3.0" TO W-FELD   PERFORM VC-ZEILE
           MOVE "UID:adrv-" TO W-FELD     PERFORM TEXT-ANHAENGEN
           MOVE KT-NR TO W-FELD           PERFORM VC-ZEILE
           IF KT-IST-FIRMA
               MOVE "N:;;;;" TO W-FELD    PERFORM VC-ZEILE
               MOVE "FN:" TO W-FELD       PERFORM TEXT-ANHAENGEN
               MOVE KF-NAME TO W-FELD     PERFORM WERT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
               MOVE "ORG:" TO W-FELD      PERFORM TEXT-ANHAENGEN
               MOVE KF-NAME TO W-FELD     PERFORM WERT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
               MOVE "X-ABShowAs:COMPANY" TO W-FELD
               PERFORM VC-ZEILE
               IF KF-WEBSITE NOT = SPACES
                   MOVE "URL:" TO W-FELD  PERFORM TEXT-ANHAENGEN
                   MOVE KF-WEBSITE TO W-FELD
                   PERFORM WERT-ANHAENGEN
                   PERFORM ZEILE-AUSGEBEN
               END-IF
           ELSE
               PERFORM VCARD-PERSON
           END-IF
           PERFORM VC-KATEGORIEN
           MOVE KT-NR TO W-NR-FUER-LADEN
           PERFORM ADRESSEN-LADEN
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > W-ADR-ANZ
               MOVE W-ADR(W-J) TO AD-SATZ
               PERFORM VC-ADRESSE
           END-PERFORM
           PERFORM KOMM-LADEN
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > W-KOM-ANZ
               MOVE W-KOM(W-J) TO KO-SATZ
               PERFORM VC-KOMM
           END-PERFORM
           IF KT-NOTIZ(1) NOT = SPACES OR KT-NOTIZ(2) NOT = SPACES
              OR KT-NOTIZ(3) NOT = SPACES OR KT-NOTIZ(4) NOT = SPACES
               MOVE "NOTE:" TO W-FELD     PERFORM TEXT-ANHAENGEN
               PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 4
                   IF KT-NOTIZ(W-J) NOT = SPACES
                       IF W-J > 1
                           MOVE "\n" TO W-FELD
                           PERFORM TEXT-ANHAENGEN
                       END-IF
                       MOVE KT-NOTIZ(W-J) TO W-FELD
                       PERFORM WERT-ANHAENGEN
                   END-IF
               END-PERFORM
               PERFORM ZEILE-AUSGEBEN
           END-IF
           MOVE "END:VCARD" TO W-FELD     PERFORM VC-ZEILE.

       VCARD-PERSON.
           MOVE "N:" TO W-FELD            PERFORM TEXT-ANHAENGEN
           PERFORM SATZ-LEEREN
           MOVE KP-VORSATZ TO W-WORT      PERFORM WORT-ANHAENGEN
           MOVE KP-NACHNAME TO W-WORT     PERFORM WORT-ANHAENGEN
           MOVE W-SATZ TO W-FELD          PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE KP-VORNAME TO W-FELD      PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE KP-VORNAMEN2 TO W-FELD    PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE KP-TITEL TO W-FELD        PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE KP-ZUSATZ TO W-FELD       PERFORM WERT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN
           MOVE "FN:" TO W-FELD           PERFORM TEXT-ANHAENGEN
           PERFORM SATZ-LEEREN
           MOVE KP-TITEL TO W-WORT        PERFORM WORT-ANHAENGEN
           MOVE KP-VORNAME TO W-WORT      PERFORM WORT-ANHAENGEN
           MOVE KP-VORSATZ TO W-WORT      PERFORM WORT-ANHAENGEN
           MOVE KP-NACHNAME TO W-WORT     PERFORM WORT-ANHAENGEN
           MOVE KP-ZUSATZ TO W-WORT       PERFORM WORT-ANHAENGEN
           MOVE W-SATZ TO W-FELD          PERFORM WERT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN
           IF KT-FIRMA-NR NOT = SPACES
               PERFORM FIRMENNAME-SUCHEN
               IF W-FI-FIRMA NOT = SPACES
                   MOVE "ORG:" TO W-FELD  PERFORM TEXT-ANHAENGEN
                   MOVE W-FI-FIRMA TO W-FELD
                   PERFORM WERT-ANHAENGEN
                   IF KP-ABTEILUNG NOT = SPACES
                       PERFORM VC-SEMI
                       MOVE KP-ABTEILUNG TO W-FELD
                       PERFORM WERT-ANHAENGEN
                   END-IF
                   PERFORM ZEILE-AUSGEBEN
               END-IF
           END-IF
           IF KP-POSITION NOT = SPACES
               MOVE "TITLE:" TO W-FELD    PERFORM TEXT-ANHAENGEN
               MOVE KP-POSITION TO W-FELD PERFORM WERT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-IF
           IF KP-GEBDATUM NOT = 0
               MOVE "BDAY:" TO W-FELD     PERFORM TEXT-ANHAENGEN
               MOVE KP-GEBDATUM TO W-DATUM
               STRING W-DATUM-X(1:4) "-" W-DATUM-X(5:2) "-"
                      W-DATUM-X(7:2) DELIMITED BY SIZE INTO W-FELD
               PERFORM TEXT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-IF.

       VC-KATEGORIEN.
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "KATEG" TO CD-LISTE
           MOVE KT-KATEGORIE TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE "CATEGORIES:" TO W-FELD   PERFORM TEXT-ANHAENGEN
           MOVE "N" TO W-ERSTES
           IF CD-GUELTIG
               MOVE CD-TEXT TO W-FELD     PERFORM WERT-ANHAENGEN
               MOVE "J" TO W-ERSTES
           END-IF
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 5
               IF KT-SCHLAGWORT(W-J) NOT = SPACES
                   IF W-ERSTES = "J"
                       MOVE "," TO W-FELD
                       PERFORM TEXT-ANHAENGEN
                   END-IF
                   MOVE KT-SCHLAGWORT(W-J) TO W-FELD
                   PERFORM WERT-ANHAENGEN
                   MOVE "J" TO W-ERSTES
               END-IF
           END-PERFORM
           IF W-ERSTES = "J"
               PERFORM ZEILE-AUSGEBEN
           ELSE
               PERFORM ZEILE-LEEREN
           END-IF.

       VC-ADRESSE.
           EVALUATE AD-TYP
               WHEN "W"   MOVE "ADR;TYPE=HOME" TO W-FELD
               WHEN "P"   MOVE "ADR;TYPE=POSTAL" TO W-FELD
               WHEN OTHER MOVE "ADR;TYPE=WORK" TO W-FELD
           END-EVALUATE
           PERFORM TEXT-ANHAENGEN
           IF AD-IST-STANDARD
               MOVE ",PREF" TO W-FELD     PERFORM TEXT-ANHAENGEN
           END-IF
           MOVE ":;" TO W-FELD            PERFORM TEXT-ANHAENGEN
           MOVE AD-ZUSATZ TO W-FELD       PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           PERFORM SATZ-LEEREN
           MOVE AD-STRASSE TO W-WORT      PERFORM WORT-ANHAENGEN
           MOVE AD-HAUSNR TO W-WORT       PERFORM WORT-ANHAENGEN
           MOVE W-SATZ TO W-FELD          PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE AD-ORT TO W-FELD          PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE AD-REGION TO W-FELD       PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE AD-PLZ TO W-FELD          PERFORM WERT-ANHAENGEN
           PERFORM VC-SEMI
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "LAND" TO CD-LISTE
           MOVE AD-LAND TO CD-CODE
           CALL "CODES" USING CD-PARM
           IF CD-GUELTIG
               MOVE CD-TEXT TO W-FELD
           ELSE
               MOVE AD-LAND TO W-FELD
           END-IF
           PERFORM WERT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN.

       VC-KOMM.
           EVALUATE KO-TYP
               WHEN "TE" MOVE "TEL;TYPE=WORK,VOICE" TO W-FELD
               WHEN "MO" MOVE "TEL;TYPE=CELL" TO W-FELD
               WHEN "FX" MOVE "TEL;TYPE=FAX" TO W-FELD
               WHEN "EM" MOVE "EMAIL;TYPE=INTERNET" TO W-FELD
               WHEN "WW" MOVE "URL" TO W-FELD
               WHEN "LI" MOVE "X-SOCIALPROFILE;TYPE=linkedin" TO W-FELD
               WHEN "SI" MOVE "X-SIGNAL" TO W-FELD
               WHEN "WA" MOVE "X-WHATSAPP" TO W-FELD
               WHEN OTHER MOVE "X-SONSTIGES" TO W-FELD
           END-EVALUATE
           PERFORM TEXT-ANHAENGEN
           IF KO-IST-BEVORZUGT AND KO-TYP NOT = "WW"
              AND KO-TYP NOT = "LI" AND KO-TYP NOT = "SI"
              AND KO-TYP NOT = "WA" AND KO-TYP NOT = "SO"
               MOVE ",PREF" TO W-FELD     PERFORM TEXT-ANHAENGEN
           END-IF
           MOVE ":" TO W-FELD             PERFORM TEXT-ANHAENGEN
           MOVE KO-WERT TO W-FELD         PERFORM WERT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN.

       VC-ZEILE.
           PERFORM TEXT-ANHAENGEN
           PERFORM ZEILE-AUSGEBEN.

       VC-SEMI.
           ADD 1 TO W-ZPOS
           MOVE ";" TO W-ZEILE(W-ZPOS:1).

      ******************************************************************
      * JSON: ein Objekt je Kontakt, Adressen und Kommunikation als
      * Felder darin
      ******************************************************************
       JSON-KONTAKT.
           IF AU-ANZAHL > 0
               MOVE "," TO W-FELD         PERFORM TEXT-ANHAENGEN
               PERFORM ZEILE-AUSGEBEN
           END-IF
           MOVE "{" TO W-FELD             PERFORM TEXT-ANHAENGEN
           MOVE "J" TO W-ERSTES
           MOVE "nr" TO W-JSCHLUESSEL     MOVE KT-NR TO W-FELD
           PERFORM JSON-TEXT
           MOVE "art" TO W-JSCHLUESSEL
           IF KT-IST-FIRMA
               MOVE "firma" TO W-FELD
           ELSE
               MOVE "person" TO W-FELD
           END-IF
           PERFORM JSON-TEXT
           MOVE "kategorie" TO W-JSCHLUESSEL
           MOVE KT-KATEGORIE TO W-FELD    PERFORM JSON-TEXT
           PERFORM JSON-SCHLAGWORTE
           MOVE "archiviert" TO W-JSCHLUESSEL
           IF KT-ARCHIVIERT
               MOVE "true" TO W-FELD
           ELSE
               MOVE "false" TO W-FELD
           END-IF
           PERFORM JSON-ROH
           MOVE "angelegt" TO W-JSCHLUESSEL
           MOVE KT-ANGELEGT TO W-ZEIT     PERFORM JSON-ZEIT
           MOVE "geaendert" TO W-JSCHLUESSEL
           MOVE KT-GEAENDERT TO W-ZEIT    PERFORM JSON-ZEIT
           IF KT-IST-FIRMA
               PERFORM JSON-FIRMA
           ELSE
               PERFORM JSON-PERSON
           END-IF
           MOVE "notiz" TO W-JSCHLUESSEL
           PERFORM JSON-SCHLUESSEL
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           MOVE "N" TO W-ERSTES
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 4
               IF KT-NOTIZ(W-J) NOT = SPACES
                   IF W-ERSTES = "J"
                       MOVE "\n" TO W-FELD
                       PERFORM TEXT-ANHAENGEN
                   END-IF
                   MOVE KT-NOTIZ(W-J) TO W-FELD
                   PERFORM WERT-ANHAENGEN
                   MOVE "J" TO W-ERSTES
               END-IF
           END-PERFORM
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           MOVE "N" TO W-ERSTES
           PERFORM ZEILE-AUSGEBEN
           PERFORM JSON-ADRESSEN
           PERFORM JSON-KOMMUNIKATION
           MOVE "}" TO W-FELD             PERFORM TEXT-ANHAENGEN.

       JSON-FIRMA.
           MOVE "name" TO W-JSCHLUESSEL
           MOVE KF-NAME TO W-FELD         PERFORM JSON-TEXT
           MOVE "zusatz" TO W-JSCHLUESSEL
           MOVE KF-ZUSATZ TO W-FELD       PERFORM JSON-TEXT
           MOVE "rechtsform" TO W-JSCHLUESSEL
           MOVE KF-RECHTSFORM TO W-FELD   PERFORM JSON-TEXT
           MOVE "branche" TO W-JSCHLUESSEL
           MOVE KF-BRANCHE TO W-FELD      PERFORM JSON-TEXT
           MOVE "website" TO W-JSCHLUESSEL
           MOVE KF-WEBSITE TO W-FELD      PERFORM JSON-TEXT
           MOVE "ust_id" TO W-JSCHLUESSEL
           MOVE KF-USTID TO W-FELD        PERFORM JSON-TEXT
           MOVE "steuernummer" TO W-JSCHLUESSEL
           MOVE KF-STEUERNR TO W-FELD     PERFORM JSON-TEXT
           MOVE "registergericht" TO W-JSCHLUESSEL
           MOVE KF-REGGERICHT TO W-FELD   PERFORM JSON-TEXT
           MOVE "hr_nr" TO W-JSCHLUESSEL
           MOVE KF-HRNR TO W-FELD         PERFORM JSON-TEXT
           MOVE "referenz" TO W-JSCHLUESSEL
           MOVE KF-REFERENZ TO W-FELD     PERFORM JSON-TEXT.

       JSON-PERSON.
           MOVE "anrede" TO W-JSCHLUESSEL
           MOVE KP-ANREDE TO W-FELD       PERFORM JSON-TEXT
           MOVE "titel" TO W-JSCHLUESSEL
           MOVE KP-TITEL TO W-FELD        PERFORM JSON-TEXT
           MOVE "vorname" TO W-JSCHLUESSEL
           MOVE KP-VORNAME TO W-FELD      PERFORM JSON-TEXT
           MOVE "weitere_vornamen" TO W-JSCHLUESSEL
           MOVE KP-VORNAMEN2 TO W-FELD    PERFORM JSON-TEXT
           MOVE "namensvorsatz" TO W-JSCHLUESSEL
           MOVE KP-VORSATZ TO W-FELD      PERFORM JSON-TEXT
           MOVE "nachname" TO W-JSCHLUESSEL
           MOVE KP-NACHNAME TO W-FELD     PERFORM JSON-TEXT
           MOVE "namenszusatz" TO W-JSCHLUESSEL
           MOVE KP-ZUSATZ TO W-FELD       PERFORM JSON-TEXT
           MOVE "geburtsdatum" TO W-JSCHLUESSEL
           IF KP-GEBDATUM = 0
               MOVE "null" TO W-FELD
               PERFORM JSON-ROH
           ELSE
               MOVE KP-GEBDATUM TO W-DATUM
               MOVE SPACES TO W-FELD
               STRING W-DATUM-X(1:4) "-" W-DATUM-X(5:2) "-"
                      W-DATUM-X(7:2) DELIMITED BY SIZE INTO W-FELD
               PERFORM JSON-TEXT
           END-IF
           MOVE "firma_nr" TO W-JSCHLUESSEL
           MOVE KT-FIRMA-NR TO W-FELD     PERFORM JSON-TEXT
           MOVE "position" TO W-JSCHLUESSEL
           MOVE KP-POSITION TO W-FELD     PERFORM JSON-TEXT
           MOVE "abteilung" TO W-JSCHLUESSEL
           MOVE KP-ABTEILUNG TO W-FELD    PERFORM JSON-TEXT.

       JSON-SCHLAGWORTE.
           MOVE "schlagwoerter" TO W-JSCHLUESSEL
           PERFORM JSON-SCHLUESSEL
           MOVE "[" TO W-FELD             PERFORM TEXT-ANHAENGEN
           MOVE "J" TO W-ERSTES
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > 5
               IF KT-SCHLAGWORT(W-J) NOT = SPACES
                   IF W-ERSTES = "N"
                       MOVE "," TO W-FELD PERFORM TEXT-ANHAENGEN
                   END-IF
                   MOVE "N" TO W-ERSTES
                   ADD 1 TO W-ZPOS
                   MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
                   MOVE KT-SCHLAGWORT(W-J) TO W-FELD
                   PERFORM WERT-ANHAENGEN
                   ADD 1 TO W-ZPOS
                   MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
               END-IF
           END-PERFORM
           MOVE "]" TO W-FELD             PERFORM TEXT-ANHAENGEN
           MOVE "N" TO W-ERSTES.

       JSON-ADRESSEN.
           MOVE ",""adressen"":[" TO W-FELD
           PERFORM TEXT-ANHAENGEN
           MOVE KT-NR TO W-NR-FUER-LADEN
           PERFORM ADRESSEN-LADEN
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > W-ADR-ANZ
               PERFORM ZEILE-AUSGEBEN
               MOVE W-ADR(W-J) TO AD-SATZ
               IF W-J > 1
                   MOVE "," TO W-FELD     PERFORM TEXT-ANHAENGEN
               END-IF
               MOVE "{" TO W-FELD         PERFORM TEXT-ANHAENGEN
               MOVE "J" TO W-ERSTES
               MOVE "typ" TO W-JSCHLUESSEL
               MOVE AD-TYP TO W-FELD      PERFORM JSON-TEXT
               MOVE "standard" TO W-JSCHLUESSEL
               IF AD-IST-STANDARD
                   MOVE "true" TO W-FELD
               ELSE
                   MOVE "false" TO W-FELD
               END-IF
               PERFORM JSON-ROH
               MOVE "strasse" TO W-JSCHLUESSEL
               MOVE AD-STRASSE TO W-FELD  PERFORM JSON-TEXT
               MOVE "hausnr" TO W-JSCHLUESSEL
               MOVE AD-HAUSNR TO W-FELD   PERFORM JSON-TEXT
               MOVE "zusatz" TO W-JSCHLUESSEL
               MOVE AD-ZUSATZ TO W-FELD   PERFORM JSON-TEXT
               MOVE "plz" TO W-JSCHLUESSEL
               MOVE AD-PLZ TO W-FELD      PERFORM JSON-TEXT
               MOVE "ort" TO W-JSCHLUESSEL
               MOVE AD-ORT TO W-FELD      PERFORM JSON-TEXT
               MOVE "ortsteil" TO W-JSCHLUESSEL
               MOVE AD-ORTSTEIL TO W-FELD PERFORM JSON-TEXT
               MOVE "region" TO W-JSCHLUESSEL
               MOVE AD-REGION TO W-FELD   PERFORM JSON-TEXT
               MOVE "land" TO W-JSCHLUESSEL
               MOVE AD-LAND TO W-FELD     PERFORM JSON-TEXT
               MOVE "}" TO W-FELD         PERFORM TEXT-ANHAENGEN
           END-PERFORM
           MOVE "]" TO W-FELD             PERFORM TEXT-ANHAENGEN.

       JSON-KOMMUNIKATION.
           MOVE ",""kommunikation"":[" TO W-FELD
           PERFORM TEXT-ANHAENGEN
           PERFORM KOMM-LADEN
           PERFORM VARYING W-J FROM 1 BY 1 UNTIL W-J > W-KOM-ANZ
               PERFORM ZEILE-AUSGEBEN
               MOVE W-KOM(W-J) TO KO-SATZ
               IF W-J > 1
                   MOVE "," TO W-FELD     PERFORM TEXT-ANHAENGEN
               END-IF
               MOVE "{" TO W-FELD         PERFORM TEXT-ANHAENGEN
               MOVE "J" TO W-ERSTES
               MOVE "typ" TO W-JSCHLUESSEL
               MOVE KO-TYP TO W-FELD      PERFORM JSON-TEXT
               MOVE "wert" TO W-JSCHLUESSEL
               MOVE KO-WERT TO W-FELD     PERFORM JSON-TEXT
               MOVE "bezeichnung" TO W-JSCHLUESSEL
               MOVE KO-BEZ TO W-FELD      PERFORM JSON-TEXT
               MOVE "bevorzugt" TO W-JSCHLUESSEL
               IF KO-IST-BEVORZUGT
                   MOVE "true" TO W-FELD
               ELSE
                   MOVE "false" TO W-FELD
               END-IF
               PERFORM JSON-ROH
               MOVE "}" TO W-FELD         PERFORM TEXT-ANHAENGEN
           END-PERFORM
           MOVE "]" TO W-FELD             PERFORM TEXT-ANHAENGEN.

      * "schlüssel": mit Komma davor, außer beim ersten Feld
       JSON-SCHLUESSEL.
           IF W-ERSTES = "N"
               ADD 1 TO W-ZPOS
               MOVE "," TO W-ZEILE(W-ZPOS:1)
           END-IF
           MOVE "N" TO W-ERSTES
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           MOVE W-JSCHLUESSEL TO W-FELD
           PERFORM TEXT-ANHAENGEN
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           ADD 1 TO W-ZPOS
           MOVE ":" TO W-ZEILE(W-ZPOS:1).

       JSON-TEXT.
           MOVE W-FELD TO W-SATZ
           PERFORM JSON-SCHLUESSEL
           MOVE W-SATZ TO W-FELD
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1)
           PERFORM WERT-ANHAENGEN
           ADD 1 TO W-ZPOS
           MOVE QUOTE TO W-ZEILE(W-ZPOS:1).

       JSON-ROH.
           MOVE W-FELD TO W-SATZ
           PERFORM JSON-SCHLUESSEL
           MOVE W-SATZ TO W-FELD
           PERFORM TEXT-ANHAENGEN.

       JSON-ZEIT.
           MOVE SPACES TO W-FELD
           STRING W-ZEIT(1:4) "-" W-ZEIT(5:2) "-" W-ZEIT(7:2) "T"
                  W-ZEIT(9:2) ":" W-ZEIT(11:2) ":" W-ZEIT(13:2)
                  DELIMITED BY SIZE INTO W-FELD
           PERFORM JSON-TEXT.

      ******************************************************************
      * Adressen und Kommunikation eines Kontakts in Tabellen laden
      ******************************************************************
       ADRESSEN-LADEN.
           MOVE 0 TO W-ADR-ANZ
           MOVE W-NR-FUER-LADEN TO AD-KT-NR
           MOVE 0 TO AD-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "ADIO" USING IO-PARM AD-SATZ
           PERFORM UNTIL NOT IO-OK OR W-ADR-ANZ = 99
               MOVE "WEITER" TO IO-FUNKTION
               CALL "ADIO" USING IO-PARM AD-SATZ
               IF IO-OK AND AD-KT-NR = W-NR-FUER-LADEN
                   ADD 1 TO W-ADR-ANZ
                   MOVE AD-SATZ TO W-ADR(W-ADR-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM
      *    Die Schleife hat IO-STATUS verändert; der Kontaktdurchlauf
      *    prüft ihn danach wieder.
           MOVE "00" TO IO-STATUS.

       KOMM-LADEN.
           MOVE 0 TO W-KOM-ANZ
           MOVE W-NR-FUER-LADEN TO KO-KT-NR
           MOVE 0 TO KO-LFD
           MOVE "START-NR" TO IO-FUNKTION
           CALL "KOIO" USING IO-PARM KO-SATZ
           PERFORM UNTIL NOT IO-OK OR W-KOM-ANZ = 99
               MOVE "WEITER" TO IO-FUNKTION
               CALL "KOIO" USING IO-PARM KO-SATZ
               IF IO-OK AND KO-KT-NR = W-NR-FUER-LADEN
                   ADD 1 TO W-KOM-ANZ
                   MOVE KO-SATZ TO W-KOM(W-KOM-ANZ)
               ELSE
                   MOVE "10" TO IO-STATUS
               END-IF
           END-PERFORM
           MOVE "00" TO IO-STATUS.

       COPY zeile-p.
       COPY wort-p.
