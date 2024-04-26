#!/bin/bash

file_path=$1

if [ -z "$file_path" ]; then
	echo "Please provide a file path as an argument to see what the PPX will do to it."
	exit 1
fi

if ! command -v watch &>/dev/null; then
	echo "watch command not found. Please install it using Homebrew:"
	echo "brew install watch"
	exit 1
fi

/opt/homebrew/bin/watch -n 1 -c -d "./node_modules/rescript/bsc -ppx ./ppx -bs-no-builtin-ppx -reprint-source $file_path"
