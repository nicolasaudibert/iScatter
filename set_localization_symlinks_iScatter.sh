# Usage: set_localization_symlinks_interactive_scatterplots_app.sh targetLanguage

# Delete existing symlinks if any
rm -f iScatter.html
rm -f iScatter.labels.txt

# Create new symlinks
ln -s "localization/iScatter.${1}.html" "iScatter.html"
ln -s "localization/iScatter.labels.${1}.txt" "iScatter.labels.txt"

