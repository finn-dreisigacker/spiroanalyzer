#!/bin/sh
# Läuft im Cloudflare-Build: holt die fertige Browser-Version vom Branch
# "cloudflare-pages" (gebaut von der GitHub Action) nach site/.
# Wartet bis zu 15 min, bis der Build zum aktuellen Commit dort liegt.
sha=$(git rev-parse --short=7 HEAD 2>/dev/null)
# nur ein echter Commit-Hash zählt, sonst einfach den neuesten Build nehmen
printf '%s' "$sha" | grep -Eq '^[0-9a-f]{7}$' || sha=""
echo "Commit: ${sha:-unbekannt}"
i=0
while :; do
  rm -rf site
  git clone -q --depth 1 -b cloudflare-pages https://github.com/finn-dreisigacker/spiroanalyzer.git site
  if [ -z "$sha" ] || git -C site log -1 --format=%s | grep -q "$sha"; then break; fi
  i=$((i + 1))
  if [ "$i" -ge 60 ]; then echo "Build $sha nicht gefunden, nehme den neuesten."; break; fi
  echo "Warte auf GitHub-Build $sha ..."; sleep 15
done
rm -rf site/.git
