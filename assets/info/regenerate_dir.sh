#!/usr/bin/env bash

# Set the info directory
INFO_DIR="${HOME}/.emacs.d/assets/info"

cd "$INFO_DIR" || { echo "Error: Cannot access $INFO_DIR"; exit 1; }

# Remove any existing dir
[[ -f dir ]] && rm dir

echo "Processing info files..."

success=0
failed=0

# Process all info files
for file in *.info *.info.gz; do
	[[ ! -e "$file" ]] && continue
	[[ "$file" == "dir" ]] && continue

	# Skip multi-part files
	if [[ "$file" =~ -[0-9]+\.info ]]; then
		continue
	fi

	echo -n "  Adding: $file ... "

	# Try with more explicit options
	if install-info "$file" "$INFO_DIR"/dir ; then
		echo "OK"
		((success++))
	else
		echo "FAILED"
		((failed++))
	fi
done

echo ""
echo "----------------------------------------"
echo "Summary:"
echo "  Successfully added: $success files"
echo "  Failed to add: $failed files"
echo ""

# Count actual entries in the dir file
ENTRY_COUNT=$(grep -c '^\*.*:.*\.' dir 2>/dev/null || echo "0")
echo "Total entries in dir file: $ENTRY_COUNT"
echo ""

# Show first few entries as confirmation
if [[ $ENTRY_COUNT -gt 0 ]]; then
	echo "Sample of entries added:"
	grep '^\*.*:.*\.' dir | head -5
else
	echo "WARNING: No entries found in dir file!"
	echo "Showing first 20 lines of dir file for debugging:"
	head -20 dir
fi

echo ""
echo "Done! Restart Emacs and press C-h i to see the updated info directory."
