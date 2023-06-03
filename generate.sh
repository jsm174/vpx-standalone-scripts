#!/bin/bash

find_files() {
    local dir="$1"
    local pattern="$2"
    
    for file in "$dir"/*; do
        if [[ -d "$file" ]]; then
            find_files "$file" "$pattern"
        elif [[ -f "$file" && "$file" == *"$pattern" ]]; then

	    if [[ -f "$file.original" ]]; then
	       diff -w -u "$file.original" "$file" > "$file.patch"
            else
               echo "Missing $file.original"
            fi
        fi
    done
}

find_files "." ".vbs"
