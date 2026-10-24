# Adressverwaltung in COBOL
# Die Quellen sind UTF-8. Gebaut wird aus einer Latin-9-Kopie unter
# build/l9, weil die Masken von GnuCOBOL 3.2 ein Byte pro Zeichen
# erwarten (siehe CLAUDE.md).

SHELL    := /bin/bash
COBC     ?= cobc
COBFLAGS ?= -O
PREFIX   ?= $(HOME)/.local

L9       := build/l9
LOGIK    := $(sort $(wildcard src/logik/*.cob))
BILD     := src/bild/ADRV.cob \
            $(filter-out src/bild/ADRV.cob,$(sort $(wildcard src/bild/*.cob)))
COPYS    := $(wildcard copy/*.cpy)

L9_LOGIK := $(addprefix $(L9)/,$(LOGIK))
L9_BILD  := $(addprefix $(L9)/,$(BILD))
L9_COPYS := $(addprefix $(L9)/,$(COPYS))
L9_TEST  := $(L9)/test/TESTLAUF.cob

.PHONY: all test run bildtest install clean

all: build/adrv-bin build/adrv

# UTF-8 -> Latin-9, danach Festformat prüfen: Code endet in Spalte 72.
$(L9)/%: %
	@mkdir -p $(dir $@)
	@iconv -f UTF-8 -t ISO-8859-15 $< > $@ || { rm -f $@; exit 1; }
	@LC_ALL=C awk 'length > 72 { printf "%s:%d: über Spalte 72\n", \
	  "$<", FNR; f = 1 } END { exit f }' $@ || { rm -f $@; exit 1; }

build/adrv-bin: $(L9_BILD) $(L9_LOGIK) $(L9_COPYS)
	$(COBC) $(COBFLAGS) -x -I $(L9)/copy -o $@ $(L9_BILD) $(L9_LOGIK)

build/adrv: scripts/adrv.in
	sed 's|@BIN@|$(CURDIR)/build/adrv-bin|' $< > $@
	chmod +x $@

build/testlauf: $(L9_TEST) $(L9_LOGIK) $(L9_COPYS)
	$(COBC) $(COBFLAGS) -x -I $(L9)/copy -o $@ $(L9_TEST) $(L9_LOGIK)

test: build/testlauf
	rm -rf build/testdaten && mkdir -p build/testdaten
	set -o pipefail; cd build/testdaten && \
	  ADRV_DATEN=$(CURDIR)/build/testdaten ../testlauf \
	  | iconv -f ISO-8859-15 -t UTF-8
	python3 -m json.tool build/testdaten/export.json > /dev/null
	@echo "JSON-Export ist gültiges JSON"

# Startet mit Demodaten, damit die eigenen Daten unberührt bleiben.
run: all
	@test -d build/demodaten || { mkdir -p build/demodaten && \
	  ADRV_DATEN=$(CURDIR)/build/demodaten scripts/demodaten.sh; }
	ADRV_DATEN=$(CURDIR)/build/demodaten build/adrv

bildtest: all
	test/bildtest.sh

install: build/adrv-bin
	install -Dm755 build/adrv-bin $(PREFIX)/lib/adressverwaltung/adrv-bin
	sed 's|@BIN@|$(PREFIX)/lib/adressverwaltung/adrv-bin|' \
	  scripts/adrv.in > $(PREFIX)/bin/adrv
	chmod +x $(PREFIX)/bin/adrv

clean:
	rm -rf build
