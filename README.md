# Adressverwaltung in COBOL

Eine Adressverwaltung für Firmen und Personen im Terminal, geschrieben in
COBOL (GnuCOBOL), mit Bildschirmmasken wie am Großrechner und Daten in
indizierten Dateien.

```
 Adressen · Person P000001                 angelegt 24.10.26  geändert 24.10.26

 Anrede ..........: F  Frau                           F4 = Liste
 Titel ...........: Dr.
 Vorname .........: Erika
 Weitere Vornamen :
 Namensvorsatz ...:                        Namenszusatz:
 Nachname ........: Musterfrau
 Geburtsdatum ....: 05.11.1980 TT.MM.JJJJ
 Firma ...........: F000001  Beispiel Bau
 Position ........: Geschäftsführerin
 Abteilung .......:
 Kategorie .......: KU Kunde                          F4 = Liste
 Schlagwörter ....:
 Notiz ...........:



 Briefanrede .....: Sehr geehrte Frau Dr. Musterfrau,
 Standardadresse .: (keine eigene - Post geht an die Firma)
 Kommunikation ...: e.musterfrau@beispiel-bau.example  |  +49 152 28817386
F2 Speichern  F3 zurück  F4 Liste/Firma wählen  F6 Suche  F7 Anschrift
F9 Adressen  F10 Kommunikation  F11 Löschen  F12 Archiv  F1 Hilfe

```

## Was sie kann

- **Firmen und Personen** getrennt, Personen als Ansprechpartner einer
  Firma zuordenbar (mit Position und Abteilung)
- **Beliebig viele Adressen** je Kontakt (Haupt-, Rechnungs-, Liefer-,
  Postfach-, Privatadresse), eine davon Standard; Länder als ISO-Code
- **Beliebig viele Kommunikationswege** (Telefon, Mobil, E-Mail, Fax,
  Website, LinkedIn, Signal, WhatsApp), je Typ einer bevorzugt
- Anrede Herr, Frau, neutral oder keine Angabe; die **Briefanrede** ergibt
  sich daraus („Guten Tag Kim Beispiel,“ bei neutral)
- **Anschrift nach DIN 5008**, für Ansprechpartner mit der Firma darüber
  und deren Adresse
- **Suche** nach Name (Anfang oder `*teil`), PLZ, Ort, Kategorie,
  Schlagwort; „Müller“ findet man auch als „mueller“
- **Dublettenwarnung** beim Anlegen
- **Archiv** statt Löschen; endgültiges Löschen (DSGVO) samt Adressen und
  Kommunikation nur für archivierte Kontakte
- **Export** als CSV (Excel-tauglich), vCard 3.0 und JSON;
  **Import** aus CSV
- **Serienetiketten** aus einem Suchergebnis, **Geburtstagsliste**
- Prüfung von PLZ (DE, AT, CH, LI), E-Mail, USt-IdNr. und Datum

## Voraussetzungen

- GnuCOBOL 3.x mit Berkeley DB für indizierte Dateien
- `luit` und die Locale `de_DE@euro` (ISO-8859-15), siehe unten
- `make`, `iconv`; für `make bildtest` zusätzlich `tmux`

Gentoo:

```bash
echo 'dev-lang/gnucobol ~amd64' | sudo tee /etc/portage/package.accept_keywords/gnucobol
echo 'dev-lang/gnucobol berkdb' | sudo tee /etc/portage/package.use/gnucobol
echo 'de_DE@euro ISO-8859-15' | sudo tee -a /etc/locale.gen
sudo locale-gen
sudo emerge dev-lang/gnucobol x11-apps/luit
```

Debian/Ubuntu:

```bash
sudo apt install gnucobol3 make luit    # ältere Versionen: x11-utils statt luit
sudo sed -i 's/^# *de_DE@euro/de_DE@euro/' /etc/locale.gen && sudo locale-gen
```

## Bauen und starten

```bash
make            # baut build/adrv-bin und das Startskript build/adrv
make test       # Testtreiber für alle Logik-Module
make run        # startet mit erfundenen Demodaten
make bildtest   # spielt die Masken über tmux durch
make install    # nach ~/.local/bin/adrv
```

Die Daten liegen in `~/.local/share/adressverwaltung/`, ein anderes
Verzeichnis lässt sich mit `ADRV_DATEN` setzen. Für Skripte geht es auch
ohne Bildschirm:

```bash
adrv --export C ~/adressen.csv     # C = CSV, V = vCard, J = JSON
adrv --import ~/adressen.csv
```

## Bedienung

| Taste | Wirkung |
|---|---|
| Tab, Pfeile | Feld wechseln |
| F2 | speichern |
| F3, Esc | zurück (fragt bei ungespeicherten Änderungen nach) |
| F4 | Auswahlliste zum Feld unter dem Cursor; im Firmenfeld einer Person die Firmensuche |
| F5 | neu (in Listen) |
| F6 | zurück zur Suche |
| F7 | Anschrift und Briefanrede, zusätzlich in `~/adrv-anschrift.txt` |
| F8 | Ansprechpartner einer Firma |
| F9 / F10 | Adressen / Kommunikationswege |
| F11 | in Listen: Etiketten nach `~/adrv-etiketten.txt`; in Masken: endgültig löschen |
| F12 | archivieren und wieder aktivieren |
| Bild auf/ab | blättern |

## Warum Latin-9 und luit

Die Bildschirmmasken von GnuCOBOL 3.2 rechnen mit einem Byte pro Zeichen.
In UTF-8 belegt ein „ö“ zwei Bytes, auf dem Schirm wird daraus Zeichensalat.
Deshalb arbeitet das Programm intern mit ISO-8859-15: Die Quellen liegen als
UTF-8 im Repo, `make` wandelt sie vor dem Übersetzen um, und das Startskript
lässt das Programm hinter `luit` laufen, das zwischen Latin-9 und dem
UTF-8-Terminal übersetzt. Alle Dateien nach außen (CSV, vCard, JSON,
Etiketten) sind wieder UTF-8. Zeichen außerhalb von Latin-9 („ł“, „ș“)
werden beim Import durch „?“ ersetzt und im Protokoll gemeldet.

## Aufbau

```
src/logik/   Unterprogramme ohne Bildschirm (Dateizugriff, Regeln,
             Suche, Prüfungen, Export, Import), getestet mit make test
src/bild/    Bildschirmmasken; ADRV.cob ist das Hauptprogramm
copy/        Satzbeschreibungen und Parameterblöcke
test/        Testtreiber (COBOL) und Bildschirmtest (tmux)
scripts/     Startskript und Demodaten (alle Namen erfunden)
```

Gespeichert wird in vier indizierten Dateien: `kontakte.dat` (Firmen und
Personen, mit Alternativschlüsseln für Suche und Firmenzuordnung),
`adressen.dat`, `kommunikation.dat` und `zaehler.dat` für die Nummern
`F000001` und `P000001`.

## English summary

A terminal address book for companies and people, written in COBOL
(GnuCOBOL 3.x) with SCREEN SECTION forms and indexed files. It supports
multiple addresses and contact channels per entry, German postal address
formatting (DIN 5008), search, duplicate warnings, archiving and GDPR
deletion, CSV/vCard/JSON export and CSV import. The UI is German. Because
GnuCOBOL's screen I/O assumes one byte per character, the program runs in
ISO-8859-15 behind `luit`; all exported files are UTF-8.

## Lizenz

MIT, siehe [LICENSE](LICENSE).
