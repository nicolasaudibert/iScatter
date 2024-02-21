# Usage: set_localization_symlinks_interactive_scatterplots_app.sh targetLanguage

# Delete existing symlinks
rm interactive_scatterplot_explorer.html
rm interactive_scatterplot_explorer.labels.txt

# Create new symlinks
ln -s "interactive_scatterplot_explorer.${1}.html" "interactive_scatterplot_explorer.html"
ln -s "interactive_scatterplot_explorer.labels.${1}.txt" "interactive_scatterplot_explorer.labels.txt"
