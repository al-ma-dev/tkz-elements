#!/bin/bash
# scripts/make-ctan.sh — prépare l'archive CTAN de tkz-elements
#
#   bash scripts/make-ctan.sh     (ou ./scripts/make-ctan.sh, depuis n'importe où)
#
# 1. copie doc/latex/build/TKZdoc-elements-main.pdf -> doc/tkz-elements.pdf
#    dans le dossier de travail (sauf si doc/tkz-elements.pdf est plus récent)
# 2. copie le paquet dans un dossier temporaire, SANS :
#    .git .gitignore .nova "Claude outputs" scripts build/ *.mp *.table
#    .DS_Store ._*
# 3. enlève attributs étendus et ACL (macOS), permissions 755 / 644
# 4. crée $OUT_DIR/tkz-elements.zip  (dossier racine : tkz-elements/)
#    + une copie datée tkz-elements-<version>.zip pour tes archives
set -eu

# ---------- Réglages ----------
PKG="tkz-elements"
SRC="$(cd "$(dirname "$0")/.." && pwd)"   # dossier du paquet (parent de scripts/)
OUT_DIR="${OUT_DIR:-$(dirname "$SRC")/ctan}"  # par défaut : ~/Documents/WKS/Elements/ctan
DOC_PDF="doc/latex/build/TKZdoc-elements-main.pdf"
DOC_DEST="doc/${PKG}.pdf"
EXCLUDES=(.git .gitignore .nova "Claude outputs" scripts build
          '*.mp' '*.table' .DS_Store '._*')

die() { printf "Erreur : %b\n" "$*" >&2; exit 1; }

cd "$SRC" || die "dossier introuvable : $SRC"
[[ -f "latex/${PKG}.sty" ]] || die "latex/${PKG}.sty absent : est-ce bien le dossier du paquet ?"

# ---------- Version (pour le nom de la copie datée) ----------
VERSION=$(grep -m1 -E '\\ProvidesPackage' "latex/${PKG}.sty" \
          | grep -oE 'v?[0-9]+\.[0-9]+[a-z]?' | head -n1 || true)
VERSION="${VERSION:-noversion}"
VERSION="v${VERSION#v}"

# ---------- 1. Doc ----------
[[ -f "$DOC_PDF" ]] || die "$DOC_PDF introuvable : compile la doc d'abord"
if [[ -f "$DOC_DEST" && "$DOC_PDF" -ot "$DOC_DEST" ]]; then
  echo "Doc       : $DOC_PDF est plus ancien que $DOC_DEST -> on garde $DOC_DEST"
else
  cp "$DOC_PDF" "$DOC_DEST"
  echo "Doc       : $DOC_PDF -> $DOC_DEST"
fi

# ---------- 2. Copie filtrée ----------
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
STAGE="$TMP/$PKG"
mkdir -p "$STAGE"
rsync_ex=()
for e in "${EXCLUDES[@]}"; do rsync_ex+=(--exclude "$e"); done
rsync -a "${rsync_ex[@]}" ./ "$STAGE/"

# ---------- 3. Nettoyage macOS + permissions ----------
command -v xattr >/dev/null && xattr -cr "$STAGE" 2>/dev/null || true
[[ "$OSTYPE" == darwin* ]] && chmod -R -N "$STAGE" 2>/dev/null || true
find "$STAGE" -type d -exec chmod 755 {} +
find "$STAGE" -type f -exec chmod 644 {} +

# ---------- 4. Archive ----------
mkdir -p "$OUT_DIR"
ZIP="$OUT_DIR/${PKG}.zip"
rm -f "$ZIP"
( cd "$TMP" && COPYFILE_DISABLE=1 zip -q -r -X "$ZIP" "$PKG" )
cp "$ZIP" "$OUT_DIR/${PKG}-${VERSION}.zip"

# ---------- Contrôle ----------
bad=$(zipinfo -1 "$ZIP" | grep -E '(^|/)(\.DS_Store|\._|__MACOSX|\.git|\.nova|Claude outputs|scripts/|build/)|\.(mp|table)$' || true)
[[ -z "$bad" ]] || die "fichiers indésirables dans l'archive :\n$bad"
echo "Archive   : $ZIP  ($(zipinfo -1 "$ZIP" | grep -vc '/$') fichiers)"
echo "Copie     : $OUT_DIR/${PKG}-${VERSION}.zip"
echo "Contenu   :"
zipinfo -1 "$ZIP" | sed 's|^|  |'
