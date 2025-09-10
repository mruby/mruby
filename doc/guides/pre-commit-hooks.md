# pre-commit hook documentation

| Hook ID | Language | Name | Description | Version |
|---|---|---|---|---|
| identity | N/A | run identity | check your identity | N/A |
| check-hooks-apply | N/A | run check-hooks-apply | check hooks apply to the repository | N/A |
| pre-commit-hook-validator | python | run pre-commit hook validator | Validates a pre-commit-config.yaml file to ensure all hooks have 'name' and 'description' keys | N/A |
| create-pre-commit-docs | python | create pre-commit docs | creates a Markdown file with information on the pre-commit hooks | N/A |
| prettier | node | run prettier | format files with prettier | N/A |
| [gitleaks](https://github.com/gitleaks/gitleaks) | N/A | run gitleaks | detect hardcoded secrets with gitleaks | v8.27.2 |
| [oxipng](https://github.com/shssoichiro/oxipng) | N/A | run oxipng | use lossless compression to optimize PNG files | v9.1.5 |
| [check-added-large-files](https://github.com/pre-commit/pre-commit-hooks) | N/A | check added large files | Prevent giant files from being committed | v5.0.0 |
| [check-case-conflict](https://github.com/pre-commit/pre-commit-hooks) | N/A | check case conflict | Check for files with names that would conflict on a case-insensitive filesystem like MacOS HFS+ or Windows FAT | v5.0.0 |
| [check-executables-have-shebangs](https://github.com/pre-commit/pre-commit-hooks) | N/A | check executables have shebangs | Checks that non-binary executables have a proper shebang | v5.0.0 |
| [check-illegal-windows-names](https://github.com/pre-commit/pre-commit-hooks) | N/A | check illegal windows names | Check for files that cannot be created on Windows | v5.0.0 |
| [pretty-format-json](https://github.com/pre-commit/pre-commit-hooks) | N/A | pretty format json | Checks that all your JSON files are pretty | v5.0.0 |
| [check-json](https://github.com/pre-commit/pre-commit-hooks) | N/A | check json | Attempts to load all json files to verify syntax | v5.0.0 |
| [check-merge-conflict](https://github.com/pre-commit/pre-commit-hooks) | N/A | check merge conflict | Check for files that contain merge conflict strings | v5.0.0 |
| [check-shebang-scripts-are-executable](https://github.com/pre-commit/pre-commit-hooks) | N/A | check shebang scripts are executable | Checks that scripts with shebangs are executable | v5.0.0 |
| [check-vcs-permalinks](https://github.com/pre-commit/pre-commit-hooks) | N/A | check vcs permalinks | Ensures that links to vcs websites are permalinks | v5.0.0 |
| [check-yaml](https://github.com/pre-commit/pre-commit-hooks) | N/A | check yaml | Attempts to load all yaml files to verify syntax | v5.0.0 |
| [destroyed-symlinks](https://github.com/pre-commit/pre-commit-hooks) | N/A | destroyed symlinks | Detects symlinks which are changed to regular files with a content of a path which that symlink was pointing to | v5.0.0 |
| [detect-aws-credentials](https://github.com/pre-commit/pre-commit-hooks) | N/A | detect aws credentials | Checks for the existence of AWS secrets that you have set up with the AWS CLI | v5.0.0 |
| [detect-private-key](https://github.com/pre-commit/pre-commit-hooks) | N/A | detect private key | Checks for the existence of private keys | v5.0.0 |
| [end-of-file-fixer](https://github.com/pre-commit/pre-commit-hooks) | N/A | end of file fixer | Makes sure files end in a newline and only a newline | v5.0.0 |
| [file-contents-sorter](https://github.com/pre-commit/pre-commit-hooks) | N/A | file contents sorter | Sort the lines in specified files (defaults to alphabetical) | v5.0.0 |
| [fix-byte-order-marker](https://github.com/pre-commit/pre-commit-hooks) | N/A | fix byte order marker | Removes UTF-8 byte order marker | v5.0.0 |
| [forbid-submodules](https://github.com/pre-commit/pre-commit-hooks) | N/A | forbid submodules | Prevent addition of new git submodules | v5.0.0 |
| [mixed-line-ending](https://github.com/pre-commit/pre-commit-hooks) | N/A | mixed line ending | Replaces or checks mixed line ending | v5.0.0 |
| [trailing-whitespace](https://github.com/pre-commit/pre-commit-hooks) | N/A | trailing whitespace | Trims trailing whitespace | v5.0.0 |
| [forbid-tabs](https://github.com/Lucas-C/pre-commit-hooks) | N/A | run no-tabs checker | check the codebase for tabs | v1.5.5 |
| [remove-tabs](https://github.com/Lucas-C/pre-commit-hooks) | N/A | run tabs remover | find and convert tabs to spaces | v1.5.5 |
| [actionlint](https://github.com/rhysd/actionlint) | N/A | run actionlint | lint GitHub Actions workflow files | v1.7.7 |
| [codespell](https://github.com/codespell-project/codespell) | N/A | run codespell | check spelling with codespell | v2.4.1 |
| [markdownlint](https://github.com/igorshubovych/markdownlint-cli) | N/A | run markdownlint | checks the style of Markdown files | v0.45.0 |
| [markdown-link-check](https://github.com/tcort/markdown-link-check) | N/A | run markdown-link-check | checks hyperlinks in Markdown files | v3.13.7 |
| [rubocop](https://github.com/rubocop/rubocop) | N/A | run rubocop | RuboCop is a Ruby code style checker (linter) and formatter based on the community-driven Ruby Style Guide | v1.78.0 |
| [shellcheck](https://github.com/shellcheck-py/shellcheck-py) | N/A | run shellcheck | check shell scripts with a static analysis tool | v0.10.0.1 |
| [yamllint](https://github.com/adrienverge/yamllint) | N/A | run yamllint | check YAML files with yamllint | v1.37.1 |
