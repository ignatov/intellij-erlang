#!/bin/bash
#
# Script to compile Erlang debugger beam files with a specific SDK
# and store them in a specified directory.
#
# Usage:
#   ./compile_with_sdk.sh <erlang_path> <output_directory>
#
# Example:
#   ./compile_with_sdk.sh /opt/homebrew/opt/erlang@26/bin/erlc beams/otp_pre_27
#   ./compile_with_sdk.sh /opt/homebrew/Cellar/erlang/27.2.4/bin/erlc beams/otp_27_plus
#

set -e

# Check arguments
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <erlang_path> <output_directory>"
    echo "Example: $0 /opt/homebrew/opt/erlang@26/bin/erlc beams/otp_pre_27"
    exit 1
fi

ERLC_PATH="$1"
OUTPUT_DIR="$2"

# Validate erlc path
if [ ! -f "$ERLC_PATH" ]; then
    echo "Error: Erlang compiler not found at $ERLC_PATH"
    exit 1
fi

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# If output directory is relative, make it relative to SCRIPT_DIR
if [[ "$OUTPUT_DIR" != /* ]]; then
    OUTPUT_DIR="$SCRIPT_DIR/../$OUTPUT_DIR"
fi

# Files to compile
ERL_FILES=("debugnode.erl" "remote_debugger.erl" "remote_debugger_listener.erl" "remote_debugger_notifier.erl")

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Detect Erlang version
ERLANG_VERSION=$("${ERLC_PATH%erlc}erl" -version 2>&1 || echo "Unknown")
echo "Using Erlang version: $ERLANG_VERSION"
echo "Compiler path: $ERLC_PATH"
echo "Output directory: $OUTPUT_DIR"

# Compile the files
echo "Compiling Erlang files..."
cd "$SCRIPT_DIR"
$ERLC_PATH -o "$OUTPUT_DIR" "${ERL_FILES[@]}"

echo "Compilation complete."
echo "Compiled files in output directory:"
ls -la "$OUTPUT_DIR"
echo -e "\nDone!"