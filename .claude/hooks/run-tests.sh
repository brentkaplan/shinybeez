#!/usr/bin/env bash
# Post-edit hook: runs testthat after R source file changes.
# Returns test results as a systemMessage so Claude gets feedback.

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

# Only run for R files in app/ or tests/
if [[ "$FILE_PATH" != *.R ]]; then
  exit 0
fi
if [[ "$FILE_PATH" != */app/* && "$FILE_PATH" != */tests/* ]]; then
  exit 0
fi

RESULT=$(cd "$CLAUDE_PROJECT_DIR" && Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
  # Extract just the summary line
  SUMMARY=$(echo "$RESULT" | tail -5)
  echo "{\"systemMessage\": \"Tests passed after editing $(basename "$FILE_PATH"):\\n$SUMMARY\"}"
else
  # Include failure details
  FAILURES=$(echo "$RESULT" | grep -A 2 "Failure\|Error" | head -20)
  echo "{\"systemMessage\": \"Tests FAILED after editing $(basename "$FILE_PATH"):\\n$FAILURES\"}"
fi
