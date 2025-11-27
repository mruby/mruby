# mruby is using Rake (https://ruby.github.io/rake/) as a build tool.

RAKE = rake
DOCKER_COMPOSE = docker-compose
PRE_COMMIT = pre-commit

define check_command
	@command -v $(1) >/dev/null 2>&1 || { \
		echo "Error: $(1) is not installed or not in PATH."; \
		exit 1; \
	}
endef

# For colors
ifneq ($(shell tty -s),)
	CYAN := $(shell tput setaf 6)
	RESET := $(shell tput sgr0)
else
	CYAN :=
	RESET :=
endif

.PHONY: all test clean check checkinstall checkupdate composecheck composetest check_rake check_docker_compose check_pre_commit help
.DEFAULT_GOAL := all

all: check_rake ## build all targets, install (locally) in-repo
	$(RAKE)

test: check_rake all ## build and run all mruby tests
	$(RAKE) test

clean: check_rake ## clean all built and in-repo installed artifacts
	$(RAKE) clean

check: check_pre_commit ## run all pre-commit hooks against all files
	$(PRE_COMMIT) run --all-files

checkinstall: check_pre_commit ## install the pre-commit hooks
	$(PRE_COMMIT) install

checkupdate: check_pre_commit ## check the pre-commit hooks for updates
	$(PRE_COMMIT) autoupdate

composecheck: check_docker_compose check_pre_commit ## run all pre-commit hooks against all files with docker-compose
	$(DOCKER_COMPOSE) -p mruby run test $(PRE_COMMIT) run --all-files

composetest: check_docker_compose ## build and run all mruby tests with docker-compose
	$(DOCKER_COMPOSE) -p mruby run test

check_rake: ## check if Rake is installed
	$(call check_command, $(RAKE))

check_docker_compose: ## check if docker-compose is installed
	$(call check_command, $(DOCKER_COMPOSE))

check_pre_commit: ## check if pre-commit is installed
	$(call check_command, $(PRE_COMMIT))

help: ## display this help message
	@echo "Usage: make <target>"
	@echo
	@echo "Available targets:"
	@grep -E '^[a-z_-]+:.*##' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*## *"}; {printf "  $(CYAN)%-20s$(RESET) %s\n", $$1, $$2}'
