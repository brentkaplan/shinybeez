#!/usr/bin/env bash
# Post-edit hook: lints the edited R file with lintr.
# Returns lint results as a systemMessage so Claude gets feedback.

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

# Only run for R files in app/ or tests/
if [[ "$FILE_PATH" != *.R ]]; then
  exit 0
fi
if [[ "$FILE_PATH" != */app/* && "$FILE_PATH" != */tests/* ]]; then
  exit 0
fi

RESULT=$(cd "$CLAUDE_PROJECT_DIR" && Rscript -e "cat(lintr::lint('$FILE_PATH'))" 2>&1)

if [ -z "$RESULT" ]; then
  echo "{\"systemMessage\": \"lintr: no issues in $(basename "$FILE_PATH")\"}"
else
  # Trim to first 20 lines to keep output manageable
  LINT_OUTPUT=$(echo "$RESULT" | head -20)
  echo "{\"systemMessage\": \"lintr issues in $(basename "$FILE_PATH"):\\n$LINT_OUTPUT\"}"
fi
