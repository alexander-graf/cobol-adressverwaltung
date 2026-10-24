# Adressverwaltung in COBOL

Textbasierte Adressverwaltung für Firmen und Personen, GnuCOBOL 3.2,
Bildschirmmasken mit SCREEN SECTION, Daten in indizierten Dateien
(Berkeley DB). Öffentliches Repo `alexander-graf/cobol-adressverwaltung`.

## Bauen und starten

```bash
make            # build/adrv-bin und Startskript build/adrv
make test       # Testtreiber gegen die Logik-Module
make run        # startet mit Demodaten in build/demodaten
make bildtest   # Maskentest über tmux (nur lokal)
make install    # nach ~/.local/bin/adrv
```

## Warum Latin-9 und luit (nicht ändern ohne Grund)

Die Masken von GnuCOBOL 3.2 behandeln ein Byte als eine Zeichenzelle
(`libcob/screenio.c`, Ausgabe per `addch`). UTF-8-Umlaute erscheinen dort
als zwei kaputte Zellen. Deshalb:

- Quellen liegen als **UTF-8** im Repo (lesbar auf GitHub).
- `make` wandelt sie mit `iconv` nach `build/l9/` in **ISO-8859-15** um und
  kompiliert von dort. Dabei wird die 72-Spalten-Grenze geprüft.
- Daten werden in Latin-9 gespeichert; `PIC X(30)` heißt also 30 Zeichen.
- Das Startskript `adrv` startet das Programm hinter `luit -encoding
  ISO-8859-15` mit `LC_ALL=de_DE@euro`.
- `ADRV.cob` ruft beim Start `setlocale(LC_CTYPE, "")` auf. Ohne diesen
  Aufruf zeigt ncurses Umlaute als Leerzeichen.
- Alle Dateien nach außen (CSV, vCard, JSON, Etiketten, Anschrift) sind
  UTF-8; die Umwandlung macht das COBOL-Modul `ZEICHEN`.
- Bild↑/Bild↓/Esc liefern nur mit `COB_SCREEN_EXCEPTIONS=Y` und
  `COB_SCREEN_ESC=Y` einen CRT STATUS; setzt das Startskript.

Einrichtung auf alexBox (2026-10-24): `dev-lang/gnucobol` mit USE
`berkdb json xml` (`/etc/portage/package.{accept_keywords,use}/gnucobol`),
`x11-apps/luit`, Locale `de_DE@euro ISO-8859-15` in `/etc/locale.gen`.

## Aufbau

- `src/logik/` Unterprogramme ohne Bildschirm, alle über `make test` geprüft
  - `KTIO`, `ADIO`, `KOIO` Dateizugriff je Datei (Funktionscode in `IO-PARM`)
  - `KTDIENST` Geschäftsregeln: Nummer, Zeitstempel, Suchschlüssel,
    Standardadresse, bevorzugter Kommunikationsweg, Archiv, DSGVO-Löschen,
    Dublettenprüfung, Anschrift
  - `SUCHE`, `SUCHKEY`, `PRUEF`, `CODES`, `ZEICHEN`, `ANSCHR`, `BRIEFANR`,
    `GEBLISTE`, `ETIKETT`, `EXPORT`, `IMPORT`, `NRVERG`, `ADRVPFAD`
- `src/bild/` Masken; `ADRV.cob` ist das Hauptprogramm und muss beim
  Linken als erste Quelle stehen
- `copy/` Satzbeschreibungen und Parameterblöcke

Keine Aufrufzyklen zwischen Masken bauen (BSUCHE → BPERSON → … → BSUCHE):
GnuCOBOL bricht rekursive Aufrufe nicht-rekursiver Programme ab.

## Fachliche Entscheidungen

- Firmen und Personen getrennt; eine Person gehört höchstens einer Firma.
- Nummern `F000001` / `P000001` aus `zaehler.dat`, nie wiederverwendet.
- Mehrere Adressen und Kommunikationswege je Kontakt, je eine(r) als
  Standard bzw. bevorzugt (Kommunikation: je Typ).
- Löschen = archivieren; endgültig löschen (DSGVO) nur für archivierte
  Kontakte, entfernt Adressen und Kommunikation mit.
- Suchschlüssel: Großbuchstaben, ä→AE, ß→SS, Satzzeichen → Leerzeichen.
  Präfixsuche über den Schlüssel, `*begriff` = Enthält-Suche.
- Dubletten (gleicher Suchschlüssel, PLZ gleich oder leer) nur Warnung;
  beim Import werden sie übersprungen und protokolliert.
- Keine Bankverbindung, kein Feld für das Geschlecht, kein Änderungsprotokoll.
- Testdaten sind erfunden. Niemals echte Adressen ins Repo.

## GitHub

Repo öffentlich, MIT. CI (Ubuntu, dessen GnuCOBOL-Paket) baut und führt
`make test` aus. Vor Push nachfragen.
