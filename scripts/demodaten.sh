#!/bin/sh
# Legt die erfundenen Demodaten in $ADRV_DATEN an (für make run und
# den Bildschirmtest). Bricht ab, wenn dort schon Kontakte liegen.
set -e
cd "$(dirname "$0")/.."
: "${ADRV_DATEN:?ADRV_DATEN muss gesetzt sein}"
if [ -e "$ADRV_DATEN/kontakte.dat" ]; then
    echo "demodaten: $ADRV_DATEN enthält schon Daten" >&2
    exit 1
fi
mkdir -p "$ADRV_DATEN"
ADRV_DATEN="$ADRV_DATEN" build/adrv-bin --import "$PWD/scripts/demodaten.csv"
