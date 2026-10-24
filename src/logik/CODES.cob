       IDENTIFICATION DIVISION.
       PROGRAM-ID. CODES.
      * Feste Wertelisten: Kategorie, Rechtsform, Anrede, Adresstyp,
      * Land, Kommunikationstyp. LISTE füllt die Tabelle im Parameter,
      * TEXT liefert den Text zu einem Code, PRUEF prüft einen Code.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-I                     PIC 99.
       01  W-KATEG-WERTE.
           05  FILLER PIC X(20) VALUE "KU".
           05  FILLER PIC X(30) VALUE "Kunde".
           05  FILLER PIC X(20) VALUE "IN".
           05  FILLER PIC X(30) VALUE "Interessent".
           05  FILLER PIC X(20) VALUE "LI".
           05  FILLER PIC X(30) VALUE "Lieferant".
           05  FILLER PIC X(20) VALUE "PA".
           05  FILLER PIC X(30) VALUE "Partner".
           05  FILLER PIC X(20) VALUE "PR".
           05  FILLER PIC X(30) VALUE "privat".
           05  FILLER PIC X(20) VALUE "SO".
           05  FILLER PIC X(30) VALUE "sonstige".
       01  W-KATEG REDEFINES W-KATEG-WERTE.
           05  W-KATEG-E OCCURS 6.
               10  W-KATEG-C      PIC X(20).
               10  W-KATEG-T      PIC X(30).
       01  W-RECHTSF-WERTE.
           05  FILLER PIC X(20) VALUE "GmbH".
           05  FILLER PIC X(30) VALUE "Gesellschaft mbH".
           05  FILLER PIC X(20) VALUE "UG (haftungsbeschr.)".
           05  FILLER PIC X(30) VALUE "Unternehmergesellschaft".
           05  FILLER PIC X(20) VALUE "AG".
           05  FILLER PIC X(30) VALUE "Aktiengesellschaft".
           05  FILLER PIC X(20) VALUE "GmbH & Co. KG".
           05  FILLER PIC X(30) VALUE "Kommanditgesellschaft".
           05  FILLER PIC X(20) VALUE "KG".
           05  FILLER PIC X(30) VALUE "Kommanditgesellschaft".
           05  FILLER PIC X(20) VALUE "OHG".
           05  FILLER PIC X(30) VALUE "offene Handelsgesellschaft".
           05  FILLER PIC X(20) VALUE "GbR".
           05  FILLER PIC X(30) VALUE "Gesellschaft bürgerl. Rechts".
           05  FILLER PIC X(20) VALUE "PartG".
           05  FILLER PIC X(30) VALUE "Partnerschaftsgesellschaft".
           05  FILLER PIC X(20) VALUE "e.K.".
           05  FILLER PIC X(30) VALUE "eingetragener Kaufmann".
           05  FILLER PIC X(20) VALUE "e.V.".
           05  FILLER PIC X(30) VALUE "eingetragener Verein".
           05  FILLER PIC X(20) VALUE "eG".
           05  FILLER PIC X(30) VALUE "eingetragene Genossenschaft".
           05  FILLER PIC X(20) VALUE "SE".
           05  FILLER PIC X(30) VALUE "Europäische Gesellschaft".
           05  FILLER PIC X(20) VALUE "Einzelunternehmen".
           05  FILLER PIC X(30) VALUE "ohne Zusatz im Namen".
           05  FILLER PIC X(20) VALUE "Freiberufler".
           05  FILLER PIC X(30) VALUE "ohne Zusatz im Namen".
       01  W-RECHTSF REDEFINES W-RECHTSF-WERTE.
           05  W-RECHTSF-E OCCURS 14.
               10  W-RECHTSF-C      PIC X(20).
               10  W-RECHTSF-T      PIC X(30).
       01  W-ANREDE-WERTE.
           05  FILLER PIC X(20) VALUE "H".
           05  FILLER PIC X(30) VALUE "Herr".
           05  FILLER PIC X(20) VALUE "F".
           05  FILLER PIC X(30) VALUE "Frau".
           05  FILLER PIC X(20) VALUE "N".
           05  FILLER PIC X(30) VALUE "neutral (Vor- und Nachname)".
           05  FILLER PIC X(20) VALUE "K".
           05  FILLER PIC X(30) VALUE "keine Angabe".
       01  W-ANREDE REDEFINES W-ANREDE-WERTE.
           05  W-ANREDE-E OCCURS 4.
               10  W-ANREDE-C      PIC X(20).
               10  W-ANREDE-T      PIC X(30).
       01  W-ADRTYP-WERTE.
           05  FILLER PIC X(20) VALUE "H".
           05  FILLER PIC X(30) VALUE "Haupt-/Geschäftsadresse".
           05  FILLER PIC X(20) VALUE "R".
           05  FILLER PIC X(30) VALUE "Rechnungsadresse".
           05  FILLER PIC X(20) VALUE "L".
           05  FILLER PIC X(30) VALUE "Lieferadresse".
           05  FILLER PIC X(20) VALUE "P".
           05  FILLER PIC X(30) VALUE "Postfach".
           05  FILLER PIC X(20) VALUE "W".
           05  FILLER PIC X(30) VALUE "privat".
       01  W-ADRTYP REDEFINES W-ADRTYP-WERTE.
           05  W-ADRTYP-E OCCURS 5.
               10  W-ADRTYP-C      PIC X(20).
               10  W-ADRTYP-T      PIC X(30).
       01  W-LAND-WERTE.
           05  FILLER PIC X(20) VALUE "DE".
           05  FILLER PIC X(30) VALUE "Deutschland".
           05  FILLER PIC X(20) VALUE "AT".
           05  FILLER PIC X(30) VALUE "Österreich".
           05  FILLER PIC X(20) VALUE "CH".
           05  FILLER PIC X(30) VALUE "Schweiz".
           05  FILLER PIC X(20) VALUE "LI".
           05  FILLER PIC X(30) VALUE "Liechtenstein".
           05  FILLER PIC X(20) VALUE "LU".
           05  FILLER PIC X(30) VALUE "Luxemburg".
           05  FILLER PIC X(20) VALUE "BE".
           05  FILLER PIC X(30) VALUE "Belgien".
           05  FILLER PIC X(20) VALUE "NL".
           05  FILLER PIC X(30) VALUE "Niederlande".
           05  FILLER PIC X(20) VALUE "FR".
           05  FILLER PIC X(30) VALUE "Frankreich".
           05  FILLER PIC X(20) VALUE "IT".
           05  FILLER PIC X(30) VALUE "Italien".
           05  FILLER PIC X(20) VALUE "ES".
           05  FILLER PIC X(30) VALUE "Spanien".
           05  FILLER PIC X(20) VALUE "PL".
           05  FILLER PIC X(30) VALUE "Polen".
           05  FILLER PIC X(20) VALUE "CZ".
           05  FILLER PIC X(30) VALUE "Tschechien".
           05  FILLER PIC X(20) VALUE "DK".
           05  FILLER PIC X(30) VALUE "Dänemark".
           05  FILLER PIC X(20) VALUE "SE".
           05  FILLER PIC X(30) VALUE "Schweden".
           05  FILLER PIC X(20) VALUE "GB".
           05  FILLER PIC X(30) VALUE "Vereinigtes Königreich".
           05  FILLER PIC X(20) VALUE "US".
           05  FILLER PIC X(30) VALUE "USA".
       01  W-LAND REDEFINES W-LAND-WERTE.
           05  W-LAND-E OCCURS 16.
               10  W-LAND-C      PIC X(20).
               10  W-LAND-T      PIC X(30).
       01  W-KOMMTYP-WERTE.
           05  FILLER PIC X(20) VALUE "TE".
           05  FILLER PIC X(30) VALUE "Telefon".
           05  FILLER PIC X(20) VALUE "MO".
           05  FILLER PIC X(30) VALUE "Mobil".
           05  FILLER PIC X(20) VALUE "EM".
           05  FILLER PIC X(30) VALUE "E-Mail".
           05  FILLER PIC X(20) VALUE "FX".
           05  FILLER PIC X(30) VALUE "Fax".
           05  FILLER PIC X(20) VALUE "WW".
           05  FILLER PIC X(30) VALUE "Website".
           05  FILLER PIC X(20) VALUE "LI".
           05  FILLER PIC X(30) VALUE "LinkedIn".
           05  FILLER PIC X(20) VALUE "SI".
           05  FILLER PIC X(30) VALUE "Signal".
           05  FILLER PIC X(20) VALUE "WA".
           05  FILLER PIC X(30) VALUE "WhatsApp".
           05  FILLER PIC X(20) VALUE "SO".
           05  FILLER PIC X(30) VALUE "sonstiges".
       01  W-KOMMTYP REDEFINES W-KOMMTYP-WERTE.
           05  W-KOMMTYP-E OCCURS 9.
               10  W-KOMMTYP-C      PIC X(20).
               10  W-KOMMTYP-T      PIC X(30).
       LINKAGE SECTION.
       COPY codes.
       PROCEDURE DIVISION USING CD-PARM.
           MOVE 0 TO CD-ANZAHL
           EVALUATE CD-LISTE
               WHEN "KATEG"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 6
                       ADD 1 TO CD-ANZAHL
                       MOVE W-KATEG-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
               WHEN "RECHTSF"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 14
                       ADD 1 TO CD-ANZAHL
                       MOVE W-RECHTSF-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
               WHEN "ANREDE"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 4
                       ADD 1 TO CD-ANZAHL
                       MOVE W-ANREDE-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
               WHEN "ADRTYP"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 5
                       ADD 1 TO CD-ANZAHL
                       MOVE W-ADRTYP-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
               WHEN "LAND"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 16
                       ADD 1 TO CD-ANZAHL
                       MOVE W-LAND-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
               WHEN "KOMMTYP"
                   PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9
                       ADD 1 TO CD-ANZAHL
                       MOVE W-KOMMTYP-E(W-I) TO CD-EINTRAG(W-I)
                   END-PERFORM
           END-EVALUATE
           MOVE "N" TO CD-OK
           EVALUATE CD-FUNKTION
               WHEN "LISTE"
                   MOVE "J" TO CD-OK
               WHEN "TEXT"
               WHEN "PRUEF"
                   MOVE SPACES TO CD-TEXT
                   PERFORM VARYING W-I FROM 1 BY 1
                           UNTIL W-I > CD-ANZAHL
                       IF CD-E-CODE(W-I) = CD-CODE
                           MOVE CD-E-TEXT(W-I) TO CD-TEXT
                           MOVE "J" TO CD-OK
                       END-IF
                   END-PERFORM
           END-EVALUATE
           GOBACK.
