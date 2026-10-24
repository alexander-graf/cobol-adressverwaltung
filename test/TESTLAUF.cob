       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTLAUF.
      * Testtreiber für die Logik-Module. Läuft im Verzeichnis aus
      * $ADRV_DATEN, das vorher leer sein muss (make test sorgt dafür).
      * Alle Namen und Adressen sind erfunden.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ioparm.
       COPY dienst.
       COPY kontakt.
       COPY adresse.
       COPY komm.
       COPY suche.
       COPY codes.
       COPY pruef.
       COPY zeichen.
       COPY geb.
       COPY anschr.
       COPY austausch.
       01  W-TEST                  PIC X(40).
       01  W-IST                   PIC X(100).
       01  W-SOLL                  PIC X(100).
       01  W-OK                    PIC 9(4) VALUE 0.
       01  W-FEHLER                PIC 9(4) VALUE 0.
       01  W-TEXT                  PIC X(100).
       01  W-SCHLUESSEL            PIC X(40).
       01  W-BRIEF                 PIC X(80).
       01  W-FIRMA                 PIC X(7).
       01  W-PERSON                PIC X(7).
       01  W-PERSON2               PIC X(7).
       01  W-ZAHL                  PIC Z(4)9.
       01  W-VERZ                  PIC X(200).
       01  W-EXPORT-ANZ            PIC 9(5).
       PROCEDURE DIVISION.
           DISPLAY "Testlauf Adressverwaltung"
           PERFORM TEST-SUCHKEY
           PERFORM TEST-PRUEF
           PERFORM TEST-CODES
           PERFORM TEST-ZEICHEN
           PERFORM TEST-BRIEFANREDE
           PERFORM TEST-ANSCHRIFT
           PERFORM TEST-KONTAKTE
           PERFORM TEST-SUCHE
           PERFORM TEST-GEBURTSTAGE
           PERFORM TEST-ETIKETTEN
           PERFORM TEST-EXPORT-IMPORT
           PERFORM TEST-TILGEN
           PERFORM DATEIEN-SCHLIESSEN
           MOVE W-OK TO W-ZAHL
           DISPLAY " "
           DISPLAY "Bestanden: " FUNCTION TRIM(W-ZAHL)
           MOVE W-FEHLER TO W-ZAHL
           DISPLAY "Fehler:    " FUNCTION TRIM(W-ZAHL)
           IF W-FEHLER > 0
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           STOP RUN.

       PRUEFE.
           IF W-IST = W-SOLL
               ADD 1 TO W-OK
               DISPLAY "  ok      " W-TEST
           ELSE
               ADD 1 TO W-FEHLER
               DISPLAY "  FEHLER  " W-TEST
               DISPLAY "          ist:  [" FUNCTION TRIM(W-IST) "]"
               DISPLAY "          soll: [" FUNCTION TRIM(W-SOLL) "]"
           END-IF.

      ******************************************************************
       TEST-SUCHKEY.
           DISPLAY "SUCHKEY"
           MOVE "Müller-Lüdenscheidt" TO W-TEXT
           CALL "SUCHKEY" USING W-TEXT W-SCHLUESSEL
           MOVE "Umlaute und Bindestrich" TO W-TEST
           MOVE W-SCHLUESSEL TO W-IST
           MOVE "MUELLER LUEDENSCHEIDT" TO W-SOLL
           PERFORM PRUEFE
           MOVE "  Große Straße 5 " TO W-TEXT
           CALL "SUCHKEY" USING W-TEXT W-SCHLUESSEL
           MOVE "Eszett und Randleerzeichen" TO W-TEST
           MOVE W-SCHLUESSEL TO W-IST
           MOVE "GROSSE STRASSE 5" TO W-SOLL
           PERFORM PRUEFE
           MOVE "José Šimon" TO W-TEXT
           CALL "SUCHKEY" USING W-TEXT W-SCHLUESSEL
           MOVE "Akzente" TO W-TEST
           MOVE W-SCHLUESSEL TO W-IST
           MOVE "JOSE SIMON" TO W-SOLL
           PERFORM PRUEFE.

      ******************************************************************
       TEST-PRUEF.
           DISPLAY "PRUEF"
           MOVE "PLZ DE 5 Ziffern" TO W-TEST
           MOVE "PLZ" TO PR-FUNKTION MOVE "DE" TO PR-LAND
           MOVE "70173" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "PLZ DE 4 Ziffern abgelehnt" TO W-TEST
           MOVE "7017" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "PLZ AT 4 Ziffern" TO W-TEST
           MOVE "AT" TO PR-LAND
           MOVE "1010" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "PLZ NL ohne Prüfung" TO W-TEST
           MOVE "NL" TO PR-LAND
           MOVE "1012 AB" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "E-Mail gültig" TO W-TEST
           MOVE "EMAIL" TO PR-FUNKTION
           MOVE "kim.beispiel@example.org" TO PR-WERT
           PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "E-Mail ohne @" TO W-TEST
           MOVE "kim.example.org" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "E-Mail ohne Punkt nach @" TO W-TEST
           MOVE "kim@example" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "E-Mail mit Leerzeichen" TO W-TEST
           MOVE "kim b@example.org" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "USt-IdNr DE" TO W-TEST
           MOVE "USTID" TO PR-FUNKTION
           MOVE "DE123456789" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "USt-IdNr DE zu kurz" TO W-TEST
           MOVE "DE12345" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "USt-IdNr AT" TO W-TEST
           MOVE "ATU12345678" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "Datum 29.02. im Schaltjahr" TO W-TEST
           MOVE "DATUM" TO PR-FUNKTION
           MOVE "29.02.2024" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "Datum als JJJJMMTT" TO W-TEST
           MOVE PR-DATUM TO W-IST
           MOVE "20240229" TO W-SOLL PERFORM PRUEFE
           MOVE "Datum 29.02. ohne Schaltjahr" TO W-TEST
           MOVE "29.02.2023" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "Datum ohne führende Null" TO W-TEST
           MOVE "1.1.2020" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "Geburtsdatum in der Zukunft" TO W-TEST
           MOVE "GEBDAT" TO PR-FUNKTION
           MOVE "01.01.2999" TO PR-WERT PERFORM PRUEF-ERGEBNIS
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "Datum als Text" TO W-TEST
           MOVE "DATTEXT" TO PR-FUNKTION
           MOVE 19800105 TO PR-DATUM
           CALL "PRUEF" USING PR-PARM
           MOVE PR-WERT TO W-IST
           MOVE "05.01.1980" TO W-SOLL PERFORM PRUEFE.

       PRUEF-ERGEBNIS.
           CALL "PRUEF" USING PR-PARM
           MOVE PR-OK TO W-IST.

      ******************************************************************
       TEST-CODES.
           DISPLAY "CODES"
           MOVE "TEXT" TO CD-FUNKTION
           MOVE "LAND" TO CD-LISTE
           MOVE "AT" TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE "Ländertext" TO W-TEST
           MOVE CD-TEXT TO W-IST
           MOVE "Österreich" TO W-SOLL PERFORM PRUEFE
           MOVE "PRUEF" TO CD-FUNKTION
           MOVE "KATEG" TO CD-LISTE
           MOVE "XX" TO CD-CODE
           CALL "CODES" USING CD-PARM
           MOVE "Unbekannte Kategorie" TO W-TEST
           MOVE CD-OK TO W-IST
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE "LISTE" TO CD-FUNKTION
           MOVE "KOMMTYP" TO CD-LISTE
           CALL "CODES" USING CD-PARM
           MOVE "Liste Kommunikationstypen" TO W-TEST
           MOVE CD-ANZAHL TO W-IST
           MOVE "09" TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
       TEST-ZEICHEN.
           DISPLAY "ZEICHEN"
           MOVE "U" TO ZC-RICHTUNG
           MOVE "Köln 5€" TO ZC-EIN
           MOVE 7 TO ZC-EIN-LEN
           CALL "ZEICHEN" USING ZC-PARM
           MOVE "Latin-9 nach UTF-8" TO W-TEST
           MOVE ZC-AUS(1:ZC-AUS-LEN) TO W-IST
           MOVE X"4BC3B66C6E2035E282AC" TO W-SOLL PERFORM PRUEFE
           MOVE "L" TO ZC-RICHTUNG
           MOVE ZC-AUS TO ZC-EIN
           MOVE ZC-AUS-LEN TO ZC-EIN-LEN
           CALL "ZEICHEN" USING ZC-PARM
           MOVE "UTF-8 zurück nach Latin-9" TO W-TEST
           MOVE ZC-AUS(1:ZC-AUS-LEN) TO W-IST
           MOVE "Köln 5€" TO W-SOLL PERFORM PRUEFE
           MOVE X"C581C3B364C5BA" TO ZC-EIN
           MOVE 7 TO ZC-EIN-LEN
           CALL "ZEICHEN" USING ZC-PARM
           MOVE "Unbekannte Zeichen werden ?" TO W-TEST
           MOVE ZC-AUS(1:ZC-AUS-LEN) TO W-IST
           MOVE "?ód?" TO W-SOLL PERFORM PRUEFE
           MOVE "Ersetzungen gezählt" TO W-TEST
           MOVE ZC-ERSETZT TO W-IST
           MOVE "00002" TO W-SOLL PERFORM PRUEFE
           MOVE X"E2809E48616C6C6FE2809C" TO ZC-EIN
           MOVE 11 TO ZC-EIN-LEN
           CALL "ZEICHEN" USING ZC-PARM
           MOVE "Typografische Anführungszeichen" TO W-TEST
           MOVE ZC-AUS(1:ZC-AUS-LEN) TO W-IST
           MOVE """Hallo""" TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
       TEST-BRIEFANREDE.
           DISPLAY "BRIEFANR"
           PERFORM PERSON-VORBEREITEN
           MOVE "H" TO KP-ANREDE
           MOVE "Dr." TO KP-TITEL
           CALL "BRIEFANR" USING KT-SATZ W-BRIEF
           MOVE "Herr mit Titel" TO W-TEST
           MOVE W-BRIEF TO W-IST
           MOVE "Sehr geehrter Herr Dr. Musterfrau," TO W-SOLL
           PERFORM PRUEFE
           MOVE "N" TO KP-ANREDE
           MOVE SPACES TO KP-TITEL
           CALL "BRIEFANR" USING KT-SATZ W-BRIEF
           MOVE "Neutrale Anrede" TO W-TEST
           MOVE W-BRIEF TO W-IST
           MOVE "Guten Tag Erika Musterfrau," TO W-SOLL
           PERFORM PRUEFE
           MOVE "F" TO KP-ANREDE
           MOVE "von" TO KP-VORSATZ
           CALL "BRIEFANR" USING KT-SATZ W-BRIEF
           MOVE "Frau mit Namensvorsatz" TO W-TEST
           MOVE W-BRIEF TO W-IST
           MOVE "Sehr geehrte Frau von Musterfrau," TO W-SOLL
           PERFORM PRUEFE
           PERFORM FIRMA-VORBEREITEN
           CALL "BRIEFANR" USING KT-SATZ W-BRIEF
           MOVE "Firma" TO W-TEST
           MOVE W-BRIEF TO W-IST
           MOVE "Sehr geehrte Damen und Herren," TO W-SOLL
           PERFORM PRUEFE.

      ******************************************************************
       TEST-ANSCHRIFT.
           DISPLAY "ANSCHR"
           PERFORM FIRMA-VORBEREITEN
           PERFORM ADRESSE-VORBEREITEN
           MOVE SPACES TO AN-FIRMA1 AN-FIRMA2
           CALL "ANSCHR" USING AN-PARM KT-SATZ AD-SATZ
           MOVE "Firma: Zeilenzahl" TO W-TEST
           MOVE AN-ANZAHL TO W-IST
           MOVE "3" TO W-SOLL PERFORM PRUEFE
           MOVE "Firma: Name mit Rechtsform" TO W-TEST
           MOVE AN-ZEILE(1) TO W-IST
           MOVE "Beispiel Bau GmbH" TO W-SOLL PERFORM PRUEFE
           MOVE "Firma: Straße" TO W-TEST
           MOVE AN-ZEILE(2) TO W-IST
           MOVE "Musterweg 12a" TO W-SOLL PERFORM PRUEFE
           MOVE "Firma: Ort" TO W-TEST
           MOVE AN-ZEILE(3) TO W-IST
           MOVE "70173 Stuttgart" TO W-SOLL PERFORM PRUEFE
           MOVE "Beispiel Bau GmbH" TO KF-NAME
           CALL "ANSCHR" USING AN-PARM KT-SATZ AD-SATZ
           MOVE "Rechtsform nicht doppelt" TO W-TEST
           MOVE AN-ZEILE(1) TO W-IST
           MOVE "Beispiel Bau GmbH" TO W-SOLL PERFORM PRUEFE
           PERFORM PERSON-VORBEREITEN
           MOVE "AT" TO AD-LAND
           MOVE "1010" TO AD-PLZ
           MOVE "Wien" TO AD-ORT
           MOVE "Hinterhaus" TO AD-ZUSATZ
           CALL "ANSCHR" USING AN-PARM KT-SATZ AD-SATZ
           MOVE "Ausland: Person" TO W-TEST
           MOVE AN-ZEILE(1) TO W-IST
           MOVE "Frau Erika Musterfrau" TO W-SOLL PERFORM PRUEFE
           MOVE "Ausland: Adresszusatz" TO W-TEST
           MOVE AN-ZEILE(2) TO W-IST
           MOVE "Hinterhaus" TO W-SOLL PERFORM PRUEFE
           MOVE "Ausland: Land in Großbuchstaben" TO W-TEST
           MOVE AN-ZEILE(5) TO W-IST
           MOVE "ÖSTERREICH" TO W-SOLL PERFORM PRUEFE
           MOVE "Beispiel Bau GmbH" TO AN-FIRMA1
           MOVE "Niederlassung Süd" TO AN-FIRMA2
           CALL "ANSCHR" USING AN-PARM KT-SATZ AD-SATZ
           MOVE "Zu lang: höchstens 6 Zeilen" TO W-TEST
           MOVE AN-ANZAHL TO W-IST
           MOVE "6" TO W-SOLL PERFORM PRUEFE
           MOVE "Zu lang: Firmenzusatz fällt weg" TO W-TEST
           MOVE AN-ZEILE(2) TO W-IST
           MOVE "Frau Erika Musterfrau" TO W-SOLL PERFORM PRUEFE
           PERFORM ADRESSE-VORBEREITEN
           MOVE "P" TO AD-TYP
           MOVE "10 20 30" TO AD-STRASSE
           MOVE SPACES TO AD-HAUSNR AN-FIRMA1 AN-FIRMA2
           CALL "ANSCHR" USING AN-PARM KT-SATZ AD-SATZ
           MOVE "Postfach" TO W-TEST
           MOVE AN-ZEILE(2) TO W-IST
           MOVE "Postfach 10 20 30" TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
       TEST-KONTAKTE.
           DISPLAY "KTDIENST"
           PERFORM FIRMA-VORBEREITEN
           MOVE "NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE KT-NR TO W-FIRMA
           MOVE "Erste Firmennummer" TO W-TEST
           MOVE KT-NR TO W-IST
           MOVE "F000001" TO W-SOLL PERFORM PRUEFE
           MOVE "Suchschlüssel Firma" TO W-TEST
           MOVE KT-SUCH TO W-IST
           MOVE "BEISPIEL BAU" TO W-SOLL PERFORM PRUEFE

           PERFORM ADRESSE-VORBEREITEN
           MOVE W-FIRMA TO AD-KT-NR
           MOVE "N" TO AD-STANDARD
           MOVE "ADR-NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE "Erste Adresse wird Standard" TO W-TEST
           MOVE AD-STANDARD TO W-IST
           MOVE "J" TO W-SOLL PERFORM PRUEFE
           MOVE "R" TO AD-TYP
           MOVE "Rechnungsgasse" TO AD-STRASSE
           MOVE "J" TO AD-STANDARD
           MOVE "ADR-NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE "Zweite Adresse lfd. Nr." TO W-TEST
           MOVE AD-LFD TO W-IST
           MOVE "02" TO W-SOLL PERFORM PRUEFE
           MOVE "STDADR" TO DI-FUNKTION PERFORM DIENST
           MOVE "Neue Standardadresse" TO W-TEST
           MOVE AD-STRASSE TO W-IST
           MOVE "Rechnungsgasse" TO W-SOLL PERFORM PRUEFE
           MOVE W-FIRMA TO AD-KT-NR
           MOVE 2 TO AD-LFD
           MOVE "ADR-LOE" TO DI-FUNKTION PERFORM DIENST
           MOVE "STDADR" TO DI-FUNKTION PERFORM DIENST
           MOVE "Nach Löschen wieder erste" TO W-TEST
           MOVE AD-STRASSE TO W-IST
           MOVE "Musterweg" TO W-SOLL PERFORM PRUEFE
           MOVE "Standard-Kennzeichen zurück" TO W-TEST
           MOVE AD-STANDARD TO W-IST
           MOVE "J" TO W-SOLL PERFORM PRUEFE

           MOVE W-FIRMA TO KO-KT-NR
           MOVE "EM" TO KO-TYP
           MOVE "info@beispiel-bau.example" TO KO-WERT
           MOVE "N" TO KO-BEVORZUGT
           MOVE "KOM-NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE "vertrieb@beispiel-bau.example" TO KO-WERT
           MOVE "J" TO KO-BEVORZUGT
           MOVE "KOM-NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE "TE" TO KO-TYP
           MOVE "+49 711 000000" TO KO-WERT
           MOVE "N" TO KO-BEVORZUGT
           MOVE "KOM-NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE "EM" TO DI-KOMMTYP
           MOVE "BEVKOMM" TO DI-FUNKTION PERFORM DIENST
           MOVE "Bevorzugte E-Mail" TO W-TEST
           MOVE KO-WERT TO W-IST
           MOVE "vertrieb@beispiel-bau.example" TO W-SOLL
           PERFORM PRUEFE
           MOVE "TE" TO DI-KOMMTYP
           MOVE "BEVKOMM" TO DI-FUNKTION PERFORM DIENST
           MOVE "Einziges Telefon wird bevorzugt" TO W-TEST
           MOVE KO-BEVORZUGT TO W-IST
           MOVE "J" TO W-SOLL PERFORM PRUEFE

           PERFORM PERSON-VORBEREITEN
           MOVE W-FIRMA TO KT-FIRMA-NR
           MOVE "Geschäftsführerin" TO KP-POSITION
           MOVE 19801105 TO KP-GEBDATUM
           MOVE "NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE KT-NR TO W-PERSON
           MOVE "Erste Personennummer" TO W-TEST
           MOVE KT-NR TO W-IST
           MOVE "P000001" TO W-SOLL PERFORM PRUEFE
           MOVE "ANSCHR" TO DI-FUNKTION PERFORM DIENST
           MOVE "Ansprechpartner: Firmenzeile" TO W-TEST
           MOVE DI-ZEILE(1) TO W-IST
           MOVE "Beispiel Bau GmbH" TO W-SOLL PERFORM PRUEFE
           MOVE "Ansprechpartner: Person" TO W-TEST
           MOVE DI-ZEILE(2) TO W-IST
           MOVE "Frau Erika Musterfrau" TO W-SOLL PERFORM PRUEFE
           MOVE "Ansprechpartner: Firmenadresse" TO W-TEST
           MOVE DI-ZEILE(3) TO W-IST
           MOVE "Musterweg 12a" TO W-SOLL PERFORM PRUEFE
           MOVE "Briefanrede über Dienst" TO W-TEST
           MOVE DI-BRIEFANR TO W-IST
           MOVE "Sehr geehrte Frau Musterfrau," TO W-SOLL
           PERFORM PRUEFE

           PERFORM PERSON-VORBEREITEN
           MOVE "70173" TO DI-PLZ
           MOVE "DUBLETTE" TO DI-FUNKTION PERFORM DIENST
           MOVE "Dublette erkannt" TO W-TEST
           MOVE DI-TREFFER-NR TO W-IST
           MOVE W-PERSON TO W-SOLL PERFORM PRUEFE
           MOVE "Kim" TO KP-VORNAME
           MOVE "DUBLETTE" TO DI-FUNKTION PERFORM DIENST
           MOVE "Andere Person keine Dublette" TO W-TEST
           MOVE DI-TREFFER-NR TO W-IST
           MOVE SPACES TO W-SOLL PERFORM PRUEFE

      *    Zweite Person ohne Firma, mit eigener Adresse
           MOVE "N" TO KP-ANREDE
           MOVE "Beispiel" TO KP-NACHNAME
           MOVE "PR" TO KT-KATEGORIE
           MOVE "akquise" TO KT-SCHLAGWORT(1)
           MOVE 19901201 TO KP-GEBDATUM
           MOVE "NEU" TO DI-FUNKTION PERFORM DIENST
           MOVE KT-NR TO W-PERSON2
           PERFORM ADRESSE-VORBEREITEN
           MOVE W-PERSON2 TO AD-KT-NR
           MOVE "W" TO AD-TYP
           MOVE "10115" TO AD-PLZ
           MOVE "Berlin" TO AD-ORT
           MOVE "ADR-NEU" TO DI-FUNKTION PERFORM DIENST.

      ******************************************************************
       TEST-SUCHE.
           DISPLAY "SUCHE"
           INITIALIZE SU-PARM
           MOVE "muster" TO SU-BEGRIFF
           CALL "SUCHE" USING SU-PARM
           MOVE "Präfixsuche: Anzahl" TO W-TEST
           MOVE SU-ANZAHL TO W-IST
           MOVE "0001" TO W-SOLL PERFORM PRUEFE
           MOVE "Präfixsuche: Anzeigename" TO W-TEST
           MOVE ST-NAME(1) TO W-IST
           MOVE "Musterfrau, Erika" TO W-SOLL PERFORM PRUEFE
           MOVE "Präfixsuche: Person ohne Adresse" TO W-TEST
           MOVE ST-ORT(1) TO W-IST
           MOVE SPACES TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "*bau" TO SU-BEGRIFF
           CALL "SUCHE" USING SU-PARM
           MOVE "Enthält-Suche" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-FIRMA TO W-SOLL PERFORM PRUEFE
           MOVE "Enthält-Suche: Ort" TO W-TEST
           MOVE ST-ORT(1) TO W-IST
           MOVE "Stuttgart" TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "P" TO SU-ART
           CALL "SUCHE" USING SU-PARM
           MOVE "Nur Personen" TO W-TEST
           MOVE SU-ANZAHL TO W-IST
           MOVE "0002" TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "101" TO SU-PLZ
           CALL "SUCHE" USING SU-PARM
           MOVE "PLZ-Anfang" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-PERSON2 TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "stutt" TO SU-ORT
           CALL "SUCHE" USING SU-PARM
           MOVE "Ort-Anfang" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-FIRMA TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "AKQUISE" TO SU-SCHLAGWORT
           CALL "SUCHE" USING SU-PARM
           MOVE "Schlagwort" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-PERSON2 TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE "PR" TO SU-KATEGORIE
           CALL "SUCHE" USING SU-PARM
           MOVE "Kategorie" TO W-TEST
           MOVE SU-ANZAHL TO W-IST
           MOVE "0001" TO W-SOLL PERFORM PRUEFE
           INITIALIZE SU-PARM
           MOVE W-FIRMA TO SU-FIRMA-NR
           CALL "SUCHE" USING SU-PARM
           MOVE "Ansprechpartner der Firma" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-PERSON TO W-SOLL PERFORM PRUEFE
      *    Archivieren blendet aus der normalen Suche aus
           MOVE W-PERSON2 TO KT-NR
           PERFORM LESEN
           MOVE "ARCHIV" TO DI-FUNKTION PERFORM DIENST
           INITIALIZE SU-PARM
           MOVE "P" TO SU-ART
           CALL "SUCHE" USING SU-PARM
           MOVE "Archivierte ausgeblendet" TO W-TEST
           MOVE SU-ANZAHL TO W-IST
           MOVE "0001" TO W-SOLL PERFORM PRUEFE
           MOVE "J" TO SU-ARCHIV
           CALL "SUCHE" USING SU-PARM
           MOVE "Archivsuche" TO W-TEST
           MOVE ST-NR(1) TO W-IST
           MOVE W-PERSON2 TO W-SOLL PERFORM PRUEFE
           MOVE W-PERSON2 TO KT-NR
           PERFORM LESEN
           MOVE "REAKTIV" TO DI-FUNKTION PERFORM DIENST.

      ******************************************************************
       TEST-GEBURTSTAGE.
           DISPLAY "GEBLISTE"
           MOVE 20261024 TO GL-HEUTE
           MOVE 30 TO GL-TAGE
           CALL "GEBLISTE" USING GL-PARM
           MOVE "Nur Geburtstage in 30 Tagen" TO W-TEST
           MOVE GL-ANZAHL TO W-IST
           MOVE "001" TO W-SOLL PERFORM PRUEFE
           MOVE "In Tagen" TO W-TEST
           MOVE GL-IN-TAGEN(1) TO W-IST
           MOVE "012" TO W-SOLL PERFORM PRUEFE
           MOVE "Wird Jahre" TO W-TEST
           MOVE GL-WIRD(1) TO W-IST
           MOVE "046" TO W-SOLL PERFORM PRUEFE
           MOVE 60 TO GL-TAGE
           CALL "GEBLISTE" USING GL-PARM
           MOVE "Sortiert nach Abstand" TO W-TEST
           MOVE GL-NR(2) TO W-IST
           MOVE W-PERSON2 TO W-SOLL PERFORM PRUEFE
           MOVE 20261201 TO GL-HEUTE
           CALL "GEBLISTE" USING GL-PARM
           MOVE "Geburtstag heute = 0 Tage" TO W-TEST
           MOVE GL-IN-TAGEN(1) TO W-IST
           MOVE "000" TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
       TEST-ETIKETTEN.
           DISPLAY "ETIKETT"
           INITIALIZE SU-PARM
           CALL "SUCHE" USING SU-PARM
           INITIALIZE AU-PARM
           MOVE "etiketten.txt" TO AU-DATEI
           MOVE "J" TO AU-MIT-ANREDE
           CALL "ETIKETT" USING SU-PARM AU-PARM
           MOVE "Etiketten für alle Treffer" TO W-TEST
           MOVE AU-ANZAHL TO W-IST
           MOVE "00003" TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
       TEST-EXPORT-IMPORT.
           DISPLAY "EXPORT / IMPORT"
           INITIALIZE AU-PARM
           MOVE "C" TO AU-FORMAT
           MOVE "export.csv" TO AU-DATEI
           CALL "EXPORT" USING AU-PARM
           MOVE AU-ANZAHL TO W-EXPORT-ANZ
           MOVE "CSV-Export" TO W-TEST
           MOVE AU-ANZAHL TO W-IST
           MOVE "00003" TO W-SOLL PERFORM PRUEFE
           MOVE "V" TO AU-FORMAT
           MOVE "export.vcf" TO AU-DATEI
           CALL "EXPORT" USING AU-PARM
           MOVE "vCard-Export" TO W-TEST
           MOVE AU-ANZAHL TO W-IST
           MOVE "00003" TO W-SOLL PERFORM PRUEFE
           MOVE "J" TO AU-FORMAT
           MOVE "export.json" TO AU-DATEI
           CALL "EXPORT" USING AU-PARM
           MOVE "JSON-Export" TO W-TEST
           MOVE AU-ANZAHL TO W-IST
           MOVE "00003" TO W-SOLL PERFORM PRUEFE
      *    Gleicher Bestand: alles sind Dubletten
           INITIALIZE AU-PARM
           MOVE "export.csv" TO AU-DATEI
           MOVE "import1.txt" TO AU-PROTOKOLL
           CALL "IMPORT" USING AU-PARM
           MOVE "Import in gleichen Bestand" TO W-TEST
           MOVE AU-UEBERSPRUNGEN TO W-IST
           MOVE W-EXPORT-ANZ TO W-SOLL PERFORM PRUEFE
      *    Leerer Bestand im Unterverzeichnis "neu"
           PERFORM DATEIEN-SCHLIESSEN
           ACCEPT W-VERZ FROM ENVIRONMENT "ADRV_DATEN"
           STRING FUNCTION TRIM(W-VERZ) "/neu" DELIMITED BY SIZE
             INTO W-TEXT
           CALL "CBL_CREATE_DIR" USING W-TEXT
           SET ENVIRONMENT "ADRV_DATEN" TO W-TEXT
           INITIALIZE AU-PARM
           STRING FUNCTION TRIM(W-VERZ) "/export.csv"
                  DELIMITED BY SIZE INTO AU-DATEI
           MOVE "import2.txt" TO AU-PROTOKOLL
           CALL "IMPORT" USING AU-PARM
           MOVE "Import in leeren Bestand" TO W-TEST
           MOVE AU-ANZAHL TO W-IST
           MOVE W-EXPORT-ANZ TO W-SOLL PERFORM PRUEFE
           MOVE "P000001" TO KT-NR
           PERFORM LESEN
           MOVE "Import: Firma zugeordnet" TO W-TEST
           MOVE KT-FIRMA-NR TO W-IST
           MOVE "F000001" TO W-SOLL PERFORM PRUEFE
           MOVE "Import: Umlaute erhalten" TO W-TEST
           MOVE KP-POSITION TO W-IST
           MOVE "Geschäftsführerin" TO W-SOLL PERFORM PRUEFE
           MOVE "Import: Geburtsdatum" TO W-TEST
           MOVE KP-GEBDATUM TO W-IST
           MOVE "19801105" TO W-SOLL PERFORM PRUEFE
           MOVE "EM" TO DI-KOMMTYP
           MOVE "F000001" TO KT-NR
           PERFORM LESEN
           MOVE "BEVKOMM" TO DI-FUNKTION PERFORM DIENST
           MOVE "Import: bevorzugte E-Mail" TO W-TEST
           MOVE KO-WERT TO W-IST
           MOVE "vertrieb@beispiel-bau.example" TO W-SOLL
           PERFORM PRUEFE
           PERFORM DATEIEN-SCHLIESSEN
           SET ENVIRONMENT "ADRV_DATEN" TO W-VERZ.

      ******************************************************************
       TEST-TILGEN.
           DISPLAY "TILGEN"
           MOVE W-FIRMA TO KT-NR
           PERFORM LESEN
           MOVE "TILGEN" TO DI-FUNKTION PERFORM DIENST
           MOVE "Firma gelöscht" TO W-TEST
           MOVE W-FIRMA TO KT-NR
           MOVE "LESEN" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           MOVE IO-STATUS TO W-IST
           MOVE "23" TO W-SOLL PERFORM PRUEFE
           MOVE "STDADR" TO DI-FUNKTION PERFORM DIENST
           MOVE "Adressen mitgelöscht" TO W-TEST
           MOVE DI-OK TO W-IST
           MOVE "N" TO W-SOLL PERFORM PRUEFE
           MOVE W-PERSON TO KT-NR
           PERFORM LESEN
           MOVE "Ansprechpartner ohne Firma" TO W-TEST
           MOVE KT-FIRMA-NR TO W-IST
           MOVE SPACES TO W-SOLL PERFORM PRUEFE.

      ******************************************************************
      * Hilfen
      ******************************************************************
       DIENST.
           CALL "KTDIENST" USING DI-PARM KT-SATZ AD-SATZ KO-SATZ
           IF NOT DI-ERFOLG
               DISPLAY "  (KTDIENST " DI-FUNKTION ": "
                       FUNCTION TRIM(DI-MELDUNG) ")"
           END-IF.

       LESEN.
           MOVE "LESEN" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           IF NOT IO-OK
               DISPLAY "  (KTIO LESEN " KT-NR ": " IO-STATUS ")"
           END-IF.

       DATEIEN-SCHLIESSEN.
           MOVE "SCHLIESS" TO IO-FUNKTION
           CALL "KTIO" USING IO-PARM KT-SATZ
           CALL "ADIO" USING IO-PARM AD-SATZ
           CALL "KOIO" USING IO-PARM KO-SATZ.

       FIRMA-VORBEREITEN.
           INITIALIZE KT-SATZ
           MOVE "F" TO KT-ART
           MOVE "KU" TO KT-KATEGORIE
           MOVE "Beispiel Bau" TO KF-NAME
           MOVE "GmbH" TO KF-RECHTSFORM
           MOVE "Bauhandwerk" TO KF-BRANCHE.

       PERSON-VORBEREITEN.
           INITIALIZE KT-SATZ
           MOVE "P" TO KT-ART
           MOVE "IN" TO KT-KATEGORIE
           MOVE "F" TO KP-ANREDE
           MOVE "Erika" TO KP-VORNAME
           MOVE "Musterfrau" TO KP-NACHNAME.

       ADRESSE-VORBEREITEN.
           INITIALIZE AD-SATZ
           MOVE "H" TO AD-TYP
           MOVE "Musterweg" TO AD-STRASSE
           MOVE "12a" TO AD-HAUSNR
           MOVE "70173" TO AD-PLZ
           MOVE "Stuttgart" TO AD-ORT
           MOVE "DE" TO AD-LAND.
