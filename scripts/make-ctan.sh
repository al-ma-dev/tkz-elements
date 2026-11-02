#!/bin/bash
# scripts/make-ctan.sh — prépare l'archive CTAN de tkz-elements
#
#   bash scripts/make-ctan.sh     (ou ./scripts/make-ctan.sh, depuis n'importe où)
#
# 1. copie doc/latex/build/TKZdoc-elements-main.pdf -> doc/tkz-elements.pdf
#    dans le dossier de travail (sauf si doc/tkz-elements.pdf est plus récent)
#    et, si le module 3D existe, la doc 3D compilée -> doc/tkz-elements-3d.pdf
#    (cherchée dans doc/latex-3d/build, puis dans "Claude outputs/3d/doc/build")
# 2. copie le paquet dans un dossier temporaire, SANS :
#    .git .gitignore .nova "Claude outputs" scripts build/ *.mp *.table
#    .DS_Store ._*  et les dossiers et fichiers des outils d'IA
#    (.claude .codegraph .mcp.json CLAUDE.md)
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
DOC3D_SRCS=("doc/latex-3d/build/${PKG}-3d.pdf" "Claude outputs/3d/doc/build/${PKG}-3d.pdf")
DOC3D_DEST="doc/${PKG}-3d.pdf"
EXCLUDES=(.git .gitignore .nova "Claude outputs" scripts build
          '*.mp' '*.table' .DS_Store '._*'
          .claude .codegraph .mcp.json CLAUDE.md)

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

# ---------- 1 bis. Doc 3D ----------
if [[ -f "latex/${PKG}-3d.sty" ]]; then
  DOC3D_PDF=""
  for f in "${DOC3D_SRCS[@]}"; do
    if [[ -f "$f" ]]; then DOC3D_PDF="$f"; break; fi
  done
  if [[ -n "$DOC3D_PDF" ]]; then
    if [[ -f "$DOC3D_DEST" && "$DOC3D_PDF" -ot "$DOC3D_DEST" ]]; then
      echo "Doc 3D    : $DOC3D_PDF est plus ancien que $DOC3D_DEST -> on garde $DOC3D_DEST"
    else
      cp "$DOC3D_PDF" "$DOC3D_DEST"
      echo "Doc 3D    : $DOC3D_PDF -> $DOC3D_DEST"
    fi
  elif [[ -f "$DOC3D_DEST" ]]; then
    echo "Doc 3D    : pas de PDF compilé trouvé -> on garde $DOC3D_DEST"
  else
    die "doc 3D introuvable (${DOC3D_SRCS[*]}) : compile tkz-elements-3d.tex d'abord"
  fi
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
bad=$(zipinfo -1 "$ZIP" | grep -E '(^|/)(\.DS_Store|\._|__MACOSX|\.git|\.nova|\.claude|\.codegraph|\.mcp\.json|CLAUDE\.md|Claude outputs|scripts/|build/)|\.(mp|table)$' || true)
[[ -z "$bad" ]] || die "fichiers indésirables dans l'archive :\n$bad"
echo "Archive   : $ZIP  ($(zipinfo -1 "$ZIP" | grep -vc '/$') fichiers)"
echo "Copie     : $OUT_DIR/${PKG}-${VERSION}.zip"
echo "Contenu   :"
zipinfo -1 "$ZIP" | sed 's|^|  |'
