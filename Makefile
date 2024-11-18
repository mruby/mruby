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

all : check_rake
	$(RAKE)
.PHONY : all

test : check_rake all
	$(RAKE) test
.PHONY : test

clean : check_rake
	$(RAKE) clean
.PHONY : clean

check : check_pre_commit
	$(PRE_COMMIT) run --all-files
.PHONY : check

checkinstall : check_pre_commit
	$(PRE_COMMIT) install
.PHONY : checkinstall

checkupdate : check_pre_commit
	$(PRE_COMMIT) autoupdate
.PHONY : checkupdate

composecheck : check_docker_compose check_pre_commit
	$(DOCKER_COMPOSE) -p mruby run test $(PRE_COMMIT) run --all-files
.PHONY : composecheck

composetest : check_docker_compose
	$(DOCKER_COMPOSE) -p mruby run test
.PHONY : composetest

check_rake:
	$(call check_command, $(RAKE))
.PHONY : check_rake

check_docker_compose:
	$(call check_command, $(DOCKER_COMPOSE))
.PHONY : check_docker_compose

check_pre_commit:
	$(call check_command, $(PRE_COMMIT))
.PHONY : check_pre_commit
