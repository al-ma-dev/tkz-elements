#!/bin/zsh
# scripts/check-release.sh — zsh only
set -euo pipefail

# ---------- Réglages ----------
REF_STY="latex/tkz-elements.sty"

# Fichiers où vérifier date & version
CHECK_FILES=(
  "latex/tkz-elements.sty"
  "README.md"
  "CHANGELOG.md"
)

# Motifs de scripts autorisés exécutables
EXEC_GLOBS=("*.sh" "*.zsh" "scripts/*.sh" "scripts/*.zsh")

# Répertoires à ignorer pour les permissions
PRUNE_DIRS=(.git)

# ---------- Utils ----------
die() { print -r -- "Error: $*" >&2; exit 1; }

# macOS : permissions octales pures (ex: 755). (Sous Linux: stat -c "%a")
perm_of() { stat -f "%OLp" "$1" }

require_nonempty() {
  local v="$1" msg="$2"
  if [[ -z "$v" ]]; then
    die "$msg"
  fi
}

# ---------- Extraction ref (date + "version X.YZa") ----------
extract_ref() {
  [[ -f "$REF_STY" ]] || die "Référence introuvable: $REF_STY"
  local line date ver_words
  line=$(grep -E '\\Provides(Package|ExplPackage)' "$REF_STY" | head -n1 || true)
  require_nonempty "$line" "Impossible de trouver \\Provides... dans: $REF_STY"

  if print -r -- "$line" | grep -q "\\ProvidesExplPackage"; then
    date=$(print -r -- "$line" | sed -nE 's/.*\{([0-9]{4}\/[0-9]{2}\/[0-9]{2})\}.*/\1/p' | head -n1)
    ver_words=$(print -r -- "$line" | sed -nE 's/.*\{([Vv]ersion[[:space:]]*[0-9]+\.[0-9]+[a-z]?)\}.*/\1/p' | head -n1)
  else
    date=$(print -r -- "$line" | sed -nE 's/.*\[([0-9]{4}\/[0-9]{2}\/[0-9]{2}).*/\1/p' | head -n1)
    ver_words=$(print -r -- "$line" \
      | sed -nE 's/.*\[(.*)\].*/\1/p' \
      | sed -nE 's/.*([Vv]ersion[[:space:]]*[0-9]+\.[0-9]+[a-z]?).*/\1/p' \
      | head -n1)
  fi

  require_nonempty "$date" "Date non trouvée dans: $REF_STY"
  require_nonempty "$ver_words" "Version non trouvée (format «version X.YZa») dans: $REF_STY"

  local ver_num ver_short
  ver_num="${ver_words#version }"; ver_num="${ver_num#Version }"
  ver_short="v${ver_num}"
  date="${date//-//}"

  print -r -- "$date|$ver_words|$ver_short|$ver_num"
}

# ---------- Vérif versions/dates ----------
check_versions_dates() {
  local ref="$1"
  local date="${ref%%|*}"
  local rest="${ref#*|}"
  local ver_words="${rest%%|*}"
  local tail="${rest#*|}"
  local ver_short="${tail%%|*}"

  print "\n— Vérification date & version —"
  print "Référence   : $date  |  $ver_words  (alt: $ver_short)"
  print "Fichiers    : ${CHECK_FILES[*]}"

  local ok=1
  for f in "${CHECK_FILES[@]}"; do
    if [[ ! -f "$f" ]]; then
      print "  [skip] $f (absent)"
      continue
    fi
    local found_date=0 found_ver=0
    grep -q -- "$date" "$f" && found_date=1 || true
    if grep -qi -- "$ver_words" "$f"; then
      found_ver=1
    elif grep -qi -- "$ver_short" "$f"; then
      found_ver=1
    fi

    if (( found_date && found_ver )); then
      print "  [ok]  $f"
    else
      ok=0
      (( ! found_date )) && print "  [!!] $f — date manquante: $date"
      (( ! found_ver  )) && print "  [!!] $f — version manquante: $ver_words (ou $ver_short)"
    fi
  done

  (( ok )) && print "\n✅ Dates & versions cohérentes." || print "\n⚠️  Des incohérences existent."
}

# ---------- Permissions ----------
check_permissions() {
  print "\n— Vérification permissions —"

  # Construire l'expression prune
  local prune_expr=()
  for d in "${PRUNE_DIRS[@]}"; do
    prune_expr+=(-path "./$d" -prune -o)
  done

  # Dossiers -> 0755
  while IFS= read -r -d '' dir; do
    perms=$(perm_of "$dir")
    if [[ "$perms" != "755" ]]; then
      print "  [fix] dir  $dir -> 0755"
      chmod 0755 "$dir"
    fi
  done < <(find . "${prune_expr[@]}" -type d -print0)

  # Scripts -> 0755
  setopt null_glob
  local -a script_set
  script_set=()
  for g in "${EXEC_GLOBS[@]}"; do
    for s in ${(~)g}; do
      [[ -f "$s" ]] || continue
      [[ "$s" != ./* ]] && s="./$s"
      script_set+=("$s")
      perms=$(perm_of "$s")
      if [[ "$perms" != "755" ]]; then
        print "  [fix] exec $s -> 0755"
        chmod 0755 "$s"
      fi
    done
  done
  unsetopt null_glob

  # Tout le reste -> 0644
  while IFS= read -r -d '' f; do
    if [[ " ${script_set[*]} " == *" $f "* ]]; then
      continue
    fi
    if [[ -x "$f" ]]; then
      print "  [fix] -x  $f -> 0644"
      chmod 0644 "$f"
    else
      perms=$(perm_of "$f")
      [[ "$perms" == "644" ]] || chmod 0644 "$f"
    fi
  done < <(find . "${prune_expr[@]}" -type f -print0)

  print "\n✅ Permissions normalisées (dirs 0755, scripts 0755, reste 0644)."
}

# ---------- CLI ----------
usage() {
  cat <<'EOF'
Usage:
  zsh scripts/check-release.sh [--perms-only|--versions-only]

Sans option : vérifie la cohérence date/version ET normalise les permissions.

Réglages :
  REF_STY      : fichier .sty de référence (date + "version X.YZa")
  CHECK_FILES  : fichiers où vérifier date & version
  EXEC_GLOBS   : motifs des scripts autorisés exécutables (0755)
  PRUNE_DIRS   : répertoires exclus du check de permissions (ex: .git)
EOF
}

main() {
  local mode="${1:-}"
  local ref; ref="$(extract_ref)"

  case "$mode" in
    --versions-only) check_versions_dates "$ref" ;;
    --perms-only)    check_permissions ;;
    "" )             check_versions_dates "$ref"; check_permissions ;;
    * )              usage; exit 2 ;;
  esac
}

main "${1:-}"
