# .latexmkrc (projet)
@default_files = ('TKZdoc-elements-main.tex');  # ← ton fichier maître
$aux_dir = 'build';
$lualatex = 'lualatex -shell-escape -synctex=1 -halt-on-error %O %S';
$do_cd    = 1;                 # comme -cd
# (optionnel) ouvrir Skim
# $pdf_previewer = 'open -a Skim';
$pdf_previewer = 'open -a Skim';

