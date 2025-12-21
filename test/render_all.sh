#!/bin/bash
# Render all DOT files to PNG

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="$SCRIPT_DIR/output"

echo "Rendering DOT files to PNG..."
echo ""

for dotfile in "$OUTPUT_DIR"/*.dot; do
    if [ -f "$dotfile" ]; then
        base=$(basename "$dotfile" .dot)
        pngfile="$OUTPUT_DIR/$base.png"
        echo "Rendering $base.dot → $base.png"
        dot -Tpng "$dotfile" -o "$pngfile"
    fi
done

echo ""
echo "Done! PNGs are in: $OUTPUT_DIR/"
echo ""

# Count files
num_dot=$(ls -1 "$OUTPUT_DIR"/*.dot 2>/dev/null | wc -l)
num_png=$(ls -1 "$OUTPUT_DIR"/*.png 2>/dev/null | wc -l)
echo "Generated $num_png PNG files from $num_dot DOT files"

# On macOS, offer to open the folder
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo ""
    read -p "Open output folder? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        open "$OUTPUT_DIR"
    fi
fi
