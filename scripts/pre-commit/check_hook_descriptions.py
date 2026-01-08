#!/usr/bin/env python3
import sys
import yaml
import os

# Configuration Constants
CONFIG_FILE = os.environ.get('PRE_COMMIT_CONFIG', '.pre-commit-config.yaml')
MIN_DESC_LENGTH = 15


def validate_hook(repo_url, hook, min_length):
  """Validates a single hook and returns an error message or None."""
  hook_id = hook.get('id', 'unknown-id')
  description = hook.get('description', '')

  # Ensure description is a string and strip whitespace
  clean_desc = str(description).strip() if description is not None else ""

  if not clean_desc:
    return f"Repo: {repo_url} | Hook: {hook_id} -> Missing 'description' key."

  if len(clean_desc) < min_length:
    return (f"Repo: {repo_url} | Hook: {hook_id} -> Description too short "
            f"({len(clean_desc)} chars). Minimum required: {min_length}.")

  return None


def main():
  if not os.path.exists(CONFIG_FILE):
    print(f"ERROR: Configuration file '{CONFIG_FILE}' not found.")
    return 1

  try:
    with open(CONFIG_FILE, 'r', encoding='utf-8') as f:
      # Use CSafeLoader if available for better performance on large files
      loader = getattr(yaml, 'CSafeLoader', yaml.SafeLoader)
      config = yaml.load(f, Loader=loader)
  except yaml.YAMLError as exc:
    print(f"ERROR: Failed to parse YAML in {CONFIG_FILE}: {exc}")
    return 1

  if not config or 'repos' not in config:
    print(f"INFO: No repos found in {CONFIG_FILE}. Nothing to check.")
    return 0

  errors = []
  for repo in config.get('repos', []):
    repo_url = repo.get('repo', 'local')
    for hook in repo.get('hooks', []):
      error = validate_hook(repo_url, hook, MIN_DESC_LENGTH)
      if error:
        errors.append(error)

  if errors:
    print(f"FAIL: {len(errors)} hook(s) failed documentation requirements.")
    print("=" * 80)
    for error in errors:
      print(f"  [!] {error}")
    print("=" * 80)
    print(f"Tip: Ensure each hook has a 'description' of {MIN_DESC_LENGTH}+ chars.")
    return 1

  print(f"PASS: All hooks in {CONFIG_FILE} are properly documented.")
  return 0


if __name__ == "__main__":
  sys.exit(main())
