#!/bin/bash
# Bildschirmtest: startet die Adressverwaltung in tmux mit den
# Demodaten, spielt typische Abläufe per Tastendruck durch und prüft
# den Bildschirminhalt. Nur lokal (braucht tmux, luit, Locale).
set -u
cd "$(dirname "$0")/.."
TMP=$(mktemp -d)
trap 'tmux kill-session -t adrvtest 2>/dev/null; rm -rf "$TMP"' EXIT
OK=0; FEHLER=0

start() {
    tmux kill-session -t adrvtest 2>/dev/null
    rm -rf "$TMP/daten" "$TMP/home"; mkdir -p "$TMP/home"
    cp -r build/demodaten "$TMP/daten"
    tmux new-session -d -s adrvtest -x 80 -y 24 \
        "HOME=$TMP/home ADRV_DATEN=$TMP/daten build/adrv; sleep 30"
    sleep 1
}
k() { tmux send-keys -t adrvtest "$@"; sleep 0.4; }
bild() { tmux capture-pane -p -t adrvtest; }
erwarte() {   # erwarte "Beschreibung" "Text auf dem Schirm"
    if bild | grep -qF -- "$2"; then
        OK=$((OK + 1)); echo "  ok      $1"
    else
        FEHLER=$((FEHLER + 1)); echo "  FEHLER  $1 (erwartet: $2)"
        bild | sed 's/^/          | /'
    fi
}

[ -d build/demodaten ] || { mkdir -p build/demodaten; \
    ADRV_DATEN=$PWD/build/demodaten scripts/demodaten.sh >/dev/null; }

echo "Hauptmenü und Suche"
start
erwarte "Hauptmenü" "4   Geburtstage"
k 1 Enter; k Enter
erwarte "Firmenliste" "4 Treffer"
erwarte "Umlaute in der Liste" "Bäckerei Größenwahn e.K."
k 3 Enter
erwarte "Firma geöffnet" "Firma F000001"
erwarte "Standardadresse" "Musterweg 12a, 70173 Stuttgart"
erwarte "Ansprechpartner gezählt" "2 (F8)"
k F8
erwarte "Ansprechpartnerliste" "Musterfrau, Erika"

echo "Person anlegen, Dublette, Firma per F4"
start
k 2 Enter; k Enter; k F5
erwarte "Neue Person" "Neue Person"
k F Tab Tab Erika Tab Tab Tab Tab Musterfrau; k F2
erwarte "Dublettenwarnung" "Möglicherweise schon vorhanden: P000001"
k F2
erwarte "Trotzdem gespeichert" "Gespeichert als P000007"
k Tab Tab Beisp; k F4
erwarte "Firmensuche aus F4" "Beispiel Bau GmbH"
k 1 Enter; k F2; k F7
erwarte "Anschrift über Firma" "Frau Erika Musterfrau"
erwarte "Briefanrede" "Sehr geehrte Frau Musterfrau,"
k Enter; k F12
erwarte "Archiviert" "[ARCHIVIERT]"
k F11; k F11
erwarte "Gelöscht, zurück in der Liste" "5 Treffer"

echo "Adresse mit Prüfung"
start
k 2 Enter; k Enter; k 1 Enter; k F9; k F5
k Tab Tab Lindenstraße Tab 5 Tab Tab 1234 Tab Köln; k F2
erwarte "PLZ geprüft" "PLZ in Deutschland: 5 Ziffern"
k 50667; k F2
erwarte "Adresse gespeichert" "Lindenstraße 5, 50667 Köln"

echo "Geburtstage, Export, Hilfe"
start
k 4 Enter
erwarte "Geburtstagsliste" "Zeitraum (Tage)"
k F3; k 5 Enter; k Enter
erwarte "CSV-Export" "Kontakt(e) exportiert"
[ -s "$TMP/home/adressen.csv" ] && { OK=$((OK + 1)); echo "  ok      Exportdatei angelegt"; } \
    || { FEHLER=$((FEHLER + 1)); echo "  FEHLER  Exportdatei fehlt"; }
k F3; k F1
erwarte "Hilfe" "F4  Auswahlliste"

echo
echo "Bestanden: $OK"
echo "Fehler:    $FEHLER"
[ "$FEHLER" -eq 0 ]
