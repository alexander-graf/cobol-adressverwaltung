      * Arbeitsfelder für den Zeilenaufbau (zeile-p.cpy).
      * W-MODUS bestimmt das Maskieren in WERT-ANHAENGEN:
      *   C = CSV ("" für "), J = JSON (\" und \\), V = vCard (\, \;
      *   \\), sonst unverändert.
       01  W-ZEILE                 PIC X(8000).
       01  W-ZPOS                  PIC 9(5).
       01  W-FELD                  PIC X(300).
       01  W-FLEN                  PIC 9(3).
       01  W-FK                    PIC 9(3).
       01  W-FZ                    PIC X.
       01  W-MODUS                 PIC X.
       01  W-CRLF                  PIC X.
       01  W-ZAHL                  PIC Z(4)9.
       01  W-ZAHL2                 PIC Z(4)9.
       01  W-ZAHL3                 PIC Z(4)9.
      * Ausgabe als Bytestrom: Zeilendateien (LINE SEQUENTIAL) lehnen
      * in GnuCOBOL 3.2 Sätze mit CR ab, vCard braucht aber CRLF.
       01  W-AUS-PFAD              PIC X(251).
       01  W-AUS-HANDLE            PIC X(4).
       01  W-AUS-OFFSET            PIC X(8) COMP-X.
       01  W-AUS-ANZ               PIC X(4) COMP-X.
       01  W-AUS-FLAGS             PIC X COMP-X VALUE 0.
       01  W-AUS-ZUGRIFF           PIC X COMP-X VALUE 2.
       01  W-AUS-SPERRE            PIC X COMP-X VALUE 0.
       01  W-AUS-GERAET            PIC X COMP-X VALUE 0.
       01  W-AUS-RC                PIC S9(9).
