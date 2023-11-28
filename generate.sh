#!/bin/bash

find_table_patches() {
    for file in "$1"/*; do
        if [[ -d "$file" ]]; then
            find_table_patches "$file" ".vbs"
        elif [[ -f "$file" && "$file" == *".vbs" ]]; then
	    if [[ -f "$file.original" ]]; then
	       diff -w -u "$file.original" "$file" > "$file.patch"
            else
               echo "Missing $file.original"
            fi
        fi
    done
}

find_dmd_patches() {
    for file in "$1"/*; do
        if [[ -d "$file" ]]; then
            find_dmd_patches "$file" ".vbs"
        elif [[ -f "$file" && "$file" == *".vbs" ]]; then
            if [[ -f "$file.dmd" ]]; then
               diff -w -u "$file" "$file.dmd" > "$file.dmd.patch"
            else
               echo "Missing $file.dmd"
            fi
        fi
    done
}

find_table_patches .
find_dmd_patches .
