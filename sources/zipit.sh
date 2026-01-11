PGM=pgm/TRFVTF1B.cob
OUT=copybooks.zip

awk '
  # Skip comment lines: column 7 == *
  substr($0,7,1) == "*" { next }

  # Process only lines containing COPY (case-insensitive)
  toupper($0) ~ /COPY/ {
    line = toupper($0)

    # DDS-ALL-FORMATS OF copybook
    if (match(line, /COPY[[:space:]]+DDS-ALL-FORMATS[[:space:]]+OF[[:space:]]+([A-Z0-9_-]+)/, m)) {
      print m[1]
    }
    # Normal COPY
    else if (match(line, /COPY[[:space:]]+([A-Z0-9_-]+)/, m)) {
      print m[1]
    }
  }
' "$PGM" \
| sort -u \
| awk '{print "cpy/"$0".cpy"}' \
| xargs -r zip "$OUT"

