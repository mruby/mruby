#!/usr/bin/env python3

import yaml
import sys  # Import the sys module to access command-line arguments

# Define a constant for the default pre-commit config filename
DEFAULT_PRE_COMMIT_CONFIG_FILE = ".pre-commit-config.yaml"

def generate_pre_commit_table(yaml_path):
  """
  Generates a Markdown table from a pre-commit-config.yaml file.

  Args:
      yaml_path (str): The path to the pre-commit-config.yaml file.

  Returns:
      str: A Markdown formatted table string or an error message.
  """
  try:
    with open(yaml_path, 'r', encoding='utf-8') as f:  # Added encoding for better compatibility
      config = yaml.safe_load(f)
  except FileNotFoundError:
    return f"Error: The file '{yaml_path}' was not found."
  except yaml.YAMLError as e:
    return f"Error parsing YAML file '{yaml_path}': {e}"
  except Exception as e:
    return f"An unexpected error occurred while reading '{yaml_path}': {e}"

  table_header = "| Hook ID | Language | Name | Description | Version |\n"
  table_separator = "|---|---|---|---|---|\n"
  table_rows = []

  # Ensure config is a dictionary and has a 'repos' key that is a list
  if not isinstance(config, dict) or 'repos' not in config or not isinstance(config['repos'], list):
    return f"Error: Invalid pre-commit config structure in '{yaml_path}'. 'repos' section is missing or not a list."

  for repo_index, repo in enumerate(config.get("repos", [])):
    if not isinstance(repo, dict):
      print(f"Warning: Repository at index {repo_index} is not a valid dictionary. Skipping.")
      continue

    version = repo.get("rev", "N/A")
    url = repo.get("repo", "N/A")

    if 'hooks' not in repo or not isinstance(repo['hooks'], list):
      print(
        f"Warning: 'hooks' section not found or is not a list for repo '{url}'. Skipping hooks processing for this repo.")
      continue

    for hook_index, hook in enumerate(repo.get("hooks", [])):
      if not isinstance(hook, dict):
        print(f"Warning: Hook at index {hook_index} in repo '{url}' is not a valid dictionary. Skipping.")
        continue

      hook_id = hook.get("id", "N/A")
      name = hook.get("name", "N/A")
      description = hook.get("description", "N/A")
      language = hook.get("language", "N/A")

      # Construct the entry for the Hook ID column
      if url and url not in ["local", "meta"]:
        entry = f"[{hook_id}]({url})"
      else:
        entry = f"{hook_id}"

      table_rows.append(f"| {entry} | {language} | {name} | {description} | {version} |\n")

  if not table_rows:
    return f"No hooks found in '{yaml_path}' to generate a table."

  return table_header + table_separator + "".join(table_rows)


def create_markdown_file(target_file_path, content_to_append):
  """
  Creates or overwrites a Markdown file with the provided content.

  Args:
      target_file_path (str): The path to the output Markdown file.
      content_to_append (str): The Markdown content to write to the file.

  Returns:
      str: A success message or an error message.
  """
  try:
    # Ensure the directory exists before writing the file
    import os
    os.makedirs(os.path.dirname(target_file_path), exist_ok=True)

    with open(target_file_path, "w", encoding='utf-8') as f:  # Changed to 'w' to overwrite, added encoding
      f.write("# pre-commit hook documentation\n\n")
      f.write(content_to_append)
    return f"File content successfully created at '{target_file_path}'."
  except OSError as e:
    return f"Error creating file '{target_file_path}': {e}"
  except Exception as e:
    return f"An unexpected error occurred while writing to '{target_file_path}': {e}"


if __name__ == "__main__":
  # Check if a command-line argument for the config file path is provided
  if len(sys.argv) > 1:
    pre_commit_yaml_path = sys.argv[1]
  else:
    # Default file path if no argument is provided
    pre_commit_yaml_path = DEFAULT_PRE_COMMIT_CONFIG_FILE
    print(f"No pre-commit config file path provided. Attempting to use default: '{pre_commit_yaml_path}'")

  output_markdown_path = "doc/guides/pre-commit-hooks.md"

  # Generate the Markdown table
  markdown_table = generate_pre_commit_table(pre_commit_yaml_path)

  # Add the table to the target Markdown file if no error occurred during table generation
  if markdown_table.startswith("Error:"):
    print(markdown_table)  # Print the error message from generate_pre_commit_table
    sys.exit(1)  # Exit with a non-zero code to indicate failure
  elif markdown_table.startswith("No hooks found:"):
    print(markdown_table)  # Print the message if no hooks are found
    sys.exit(0)  # Exit with success if no hooks but no other error
  else:
    result = create_markdown_file(output_markdown_path, markdown_table)
    print(result)
    if "Error" in result:
      sys.exit(1)  # Exit with failure if create_markdown_file had an error
    else:
      sys.exit(0)  # Exit with success
