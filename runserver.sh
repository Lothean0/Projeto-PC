#!/bin/bash
# shellcheck disable=SC2164
cd "$(dirname "$0")"
# Define paths
SRC_DIR="Server/src"
BIN_DIR="out/production/Server"

# Create the BIN_DIR directory if it doesn't exist
mkdir -p "$BIN_DIR"

# Compile all Erlang files in the src directory
echo "Compiling Erlang source files..."
erlc -o "$BIN_DIR" $SRC_DIR/*.erl

# Check if compilation was successful
if [ $? -ne 0 ]; then
  echo "Compilation failed. Exiting."
  exit 1
fi

echo "Compilation successful. Starting the server..."

# Start the Erlang VM and run the server
erl -pa "$BIN_DIR" -eval "server:start(8000)."
