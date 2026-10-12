#!/bin/bash
# scripts/set-version.sh — met à jour le numéro de version et la date de tkz-elements
#
#   bash scripts/set-version.sh                 (demande la version)
#   bash scripts/set-version.sh 6.00c           (date du jour)
#   bash scripts/set-version.sh 6.00c 2026/10/15
#
# Fichiers modifiés :
#   latex/tkz-elements.sty : \ProvidesPackage{...}[DATE vX.YYc ...] et \typeout{... vX.YYc (DATE) ...}
#   README.md              : _Current release: **[DATE version X.YYc]**_
#   CHANGELOG.md           : première entrée "## [DATE — version X.YYc]"
#                            - si elle porte déjà la nouvelle version ou une version en préparation :
#                              son en-tête est mis à jour ;
#                            - si elle porte la version actuelle du .sty (déjà publiée) :
#                              une nouvelle entrée vide est ajoutée au-dessus.
set -eu

SRC="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SRC"
STY="latex/tkz-elements.sty"
README="README.md"
CHANGELOG="CHANGELOG.md"

die() { echo "Erreur : $*" >&2
        command -v osascript >/dev/null && [ ! -t 0 ] && \
          osascript -e "display alert \"set-version\" message \"$*\" as critical" >/dev/null 2>&1 || true
        exit 1; }

for f in "$STY" "$README" "$CHANGELOG"; do [ -f "$f" ] || die "$f introuvable"; done

# ---------- Versions actuelles ----------
OLD_VER=$(perl -ne 'if (/\\ProvidesPackage\{tkz-elements\}\[\S+ v(\d+\.\d+[a-z]?)/) { print $1; exit }' "$STY")
OLD_DATE=$(perl -ne 'if (/\\ProvidesPackage\{tkz-elements\}\[(\d{4}\/\d\d\/\d\d)/) { print $1; exit }' "$STY")
CL_VER=$(perl -ne 'if (/^## \[\d{4}\/\d\d\/\d\d — version (\d+\.\d+[a-z]?)\]/) { print $1; exit }' "$CHANGELOG")
[ -n "$OLD_VER" ] || die "version introuvable dans $STY"

# ---------- Nouvelle version ----------
VER="${1:-}"
DATE="${2:-$(date +%Y/%m/%d)}"
DEFAULT="$OLD_VER"; [ -n "$CL_VER" ] && [ "$CL_VER" != "$OLD_VER" ] && DEFAULT="$CL_VER"
if [ -z "$VER" ]; then
  if [ -t 0 ]; then
    read -r -p "Nouvelle version (actuelle : $OLD_VER) [$DEFAULT] : " VER
    VER="${VER:-$DEFAULT}"
  elif command -v osascript >/dev/null; then
    VER=$(osascript -e "text returned of (display dialog \"tkz-elements\nVersion actuelle : $OLD_VER du $OLD_DATE\n\nNouvelle version (date : $DATE) :\" default answer \"$DEFAULT\" with title \"set-version\")") \
      || die "annulé"
  else
    die "donne la version en argument : set-version.sh 6.00c"
  fi
fi
VER="${VER#v}"
[[ "$VER" =~ ^[0-9]+\.[0-9]{2}[a-z]$ ]] || die "version « $VER » : format attendu X.YYc (ex. 6.00c)"
[[ "$DATE" =~ ^[0-9]{4}/[0-9]{2}/[0-9]{2}$ ]] || die "date « $DATE » : format attendu AAAA/MM/JJ"

export VER DATE OLD_VER

# ---------- Vérification : chaque motif doit être trouvé une fois ----------
count() { perl -ne "\$n++ if $1; END { print \$n+0 }" "$2"; }
[ "$(count '/\\ProvidesPackage\{tkz-elements\}\[\d{4}\/\d\d\/\d\d v\d+\.\d+[a-z]?/' "$STY")" = 1 ] || die "\\ProvidesPackage non reconnu dans $STY"
[ "$(count '/\\typeout\{Package tkz-elements v\d+\.\d+[a-z]? \(\d{4}\/\d\d\/\d\d\)/' "$STY")" = 1 ] || die "\\typeout non reconnu dans $STY"
[ "$(count '/Current release: \*\*\[\d{4}\/\d\d\/\d\d version \d+\.\d+[a-z]?\]\*\*/' "$README")" = 1 ] || die "ligne « Current release » non reconnue dans $README"
[ -n "$CL_VER" ] || die "aucune entrée « ## [AAAA/MM/JJ — version X.YYc] » dans $CHANGELOG"

# ---------- Modifications ----------
perl -pi -e 's/(\\ProvidesPackage\{tkz-elements\}\[)\d{4}\/\d\d\/\d\d v\d+\.\d+[a-z]?/$1$ENV{DATE} v$ENV{VER}/;
              s/(\\typeout\{Package tkz-elements v)\d+\.\d+[a-z]? \(\d{4}\/\d\d\/\d\d\)/$1$ENV{VER} ($ENV{DATE})/' "$STY"
perl -pi -e 's/(Current release: \*\*\[)\d{4}\/\d\d\/\d\d version \d+\.\d+[a-z]?(\]\*\*)/$1$ENV{DATE} version $ENV{VER}$2/' "$README"

if [ "$CL_VER" = "$OLD_VER" ] && [ "$VER" != "$OLD_VER" ]; then
  # la première entrée est la version publiée : on ajoute une entrée au-dessus
  perl -0pi -e 's/^(## \[\d{4}\/\d\d\/\d\d — version )/## [$ENV{DATE} — version $ENV{VER}]\n\n   ### Added\n\n   ### Fixed\n\n$1/m' "$CHANGELOG"
  CL_MSG="nouvelle entrée ajoutée en tête"
else
  perl -0pi -e 's/^## \[\d{4}\/\d\d\/\d\d — version \d+\.\d+[a-z]?\]/## [$ENV{DATE} — version $ENV{VER}]/m' "$CHANGELOG"
  CL_MSG="en-tête de la première entrée mis à jour"
fi

# ---------- Résumé ----------
echo "tkz-elements : $OLD_VER ($OLD_DATE) -> $VER ($DATE)"
echo
grep -n -E '\\ProvidesPackage|\\typeout\{Package' "$STY" | sed "s|^|  $STY:|"
grep -n 'Current release' "$README" | sed "s|^|  $README:|"
grep -n -m1 '^## \[' "$CHANGELOG" | sed "s|^|  $CHANGELOG:|"
echo "  ($CL_MSG)"
