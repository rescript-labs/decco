#!/bin/bash

file_path=$1

if [ -z "$file_path" ]; then
	echo "Please provide a file path as an argument to see what the PPX will do to it."
	exit 1
fi

if ! command -v viddy &>/dev/null; then
	echo "viddy command not found. Please install it using Homebrew:"
	echo "brew install viddy"
	exit 1
fi

/opt/homebrew/bin/viddy -n 1 -c -d "./node_modules/rescript/bsc -ppx ./ppx -bs-no-builtin-ppx -reprint-source $file_path"
