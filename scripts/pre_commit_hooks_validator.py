#!/usr/bin/env python3

import yaml
import sys

# Define a constant for the default pre-commit config filename
DEFAULT_PRE_COMMIT_CONFIG_FILE = ".pre-commit-config.yaml"

def validate_pre_commit_config(file_path):
  """
  Validates a pre-commit-config.yaml file to ensure all hooks have
  'name' and 'description' keys.

  Args:
      file_path (str): The path to the pre-commit-config.yaml file.

  Returns:
      bool: True if all hooks are valid, False otherwise.
  """
  try:
    with open(file_path, 'r', encoding='utf-8') as f:
      config = yaml.safe_load(f)
  except FileNotFoundError:
    print(f"Error: The file '{file_path}' was not found.")
    return False
  except yaml.YAMLError as e:
    print(f"Error: Could not parse YAML file '{file_path}'. Please check its syntax.")
    print(f"Details: {e}")
    return False
  except Exception as e:
    print(f"An unexpected error occurred while reading the file: {e}")
    return False

  if not isinstance(config, dict):
    print(f"Error: The content of '{file_path}' is not a valid YAML dictionary.")
    return False

  if 'repos' not in config or not isinstance(config['repos'], list):
    print(f"Error: 'repos' section not found or is not a list in '{file_path}'.")
    return False

  all_hooks_valid = True
  for repo_index, repo in enumerate(config['repos']):
    if not isinstance(repo, dict):
      print(f"Warning: Repository at index {repo_index} is not a valid dictionary. Skipping.")
      all_hooks_valid = False
      continue

    repo_url = repo.get('repo', 'Unknown Repo URL')

    if 'hooks' not in repo or not isinstance(repo['hooks'], list):
      print(
        f"Warning: 'hooks' section not found or is not a list for repo '{repo_url}'. Skipping hooks validation for this repo.")
      all_hooks_valid = False
      continue

    for hook_index, hook in enumerate(repo['hooks']):
      if not isinstance(hook, dict):
        print(f"Warning: Hook at index {hook_index} in repo '{repo_url}' is not a valid dictionary. Skipping.")
        all_hooks_valid = False
        continue

      hook_id = hook.get('id', 'Unknown ID')

      # Check for 'name' key
      if 'name' not in hook:
        print(
          f"Validation Error: Hook '{hook_id}' (index {hook_index}) in repo '{repo_url}' is missing the 'name' key.")
        all_hooks_valid = False
      elif not isinstance(hook['name'], str) or not hook['name'].strip():
        print(
          f"Validation Error: Hook '{hook_id}' (index {hook_index}) in repo '{repo_url}' has an empty or invalid 'name' key.")
        all_hooks_valid = False

      # Check for 'description' key
      if 'description' not in hook:
        print(
          f"Validation Error: Hook '{hook_id}' (index {hook_index}) in repo '{repo_url}' is missing the 'description' key.")
        all_hooks_valid = False
      elif not isinstance(hook['description'], str) or not hook['description'].strip():
        print(
          f"Validation Error: Hook '{hook_id}' (index {hook_index}) in repo '{repo_url}' has an empty or invalid 'description' key.")
        all_hooks_valid = False

  return all_hooks_valid


if __name__ == "__main__":
  # Check if a command-line argument for the config file path is provided
  if len(sys.argv) > 1:
    config_file = sys.argv[1]
  else:
    # Use the defined default file path if no argument is provided
    config_file = DEFAULT_PRE_COMMIT_CONFIG_FILE
    print(f"No file path provided. Attempting to validate default file: '{config_file}'")

  if validate_pre_commit_config(config_file):
    print(f"\nValidation successful! All hooks in '{config_file}' have 'name' and 'description' keys.")
  else:
    print(f"\nValidation failed. Please check the errors listed above for '{config_file}'.")
    sys.exit(1)  # Exit with a non-zero code to indicate failure
