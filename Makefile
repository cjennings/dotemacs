# Makefile for Emacs configuration
#
# Usage:
#   make help              - Show this help message
#   make test              - Run all tests
#   make test-unit         - Run unit tests only
#   make test-file FILE=test-foo.el  - Run specific test file
#   make test-name TEST=test-foo-*   - Run tests matching pattern
#   make validate-parens   - Check for unbalanced parentheses
#   make validate-modules  - Load all modules to verify they compile
#   make compile           - Byte-compile all modules
#   make lint              - Run all linters (checkdoc, package-lint, elisp-lint)
#   make profile           - Profile Emacs startup performance
#   make clean             - Remove test artifacts and compiled files
#   make clean-compiled    - Remove .elc/.eln files only
#   make clean-tests       - Remove test artifacts only
#   make reset             - Reset to first launch (destructive!)

# Emacs binary to use (override with: make EMACS=emacs29 test)
EMACS ?= emacs

# Directories
TEST_DIR = tests
MODULE_DIR = modules
EMACS_HOME = $(HOME)/.emacs.d

# Test files
UNIT_TESTS = $(filter-out $(TEST_DIR)/test-integration-%.el, $(wildcard $(TEST_DIR)/test-*.el))
INTEGRATION_TESTS = $(wildcard $(TEST_DIR)/test-integration-%.el)
ALL_TESTS = $(UNIT_TESTS) $(INTEGRATION_TESTS)

# Module files
MODULE_FILES = $(wildcard $(MODULE_DIR)/*.el)

# Emacs batch flags
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp
EMACS_TEST = $(EMACS_BATCH) -L $(TEST_DIR) -L $(MODULE_DIR)

# Colors for output
COLOR_GREEN = \033[0;32m
COLOR_RED = \033[0;31m
COLOR_BLUE = \033[0;34m
COLOR_YELLOW = \033[0;33m
COLOR_RESET = \033[0m

.PHONY: help test test-all test-unit test-integration test-file test-name \
        validate-parens validate-modules compile lint profile \
        clean clean-compiled clean-tests reset

# Default target
.DEFAULT_GOAL := help

help:
	@echo "Emacs Configuration Targets:"
	@echo ""
	@echo "  $(COLOR_BLUE)Testing:$(COLOR_RESET)"
	@echo "    make test              - Run all tests ($(words $(ALL_TESTS)) files)"
	@echo "    make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "    make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "    make test-file FILE=<filename>  - Run specific test file"
	@echo "    make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo ""
	@echo "  $(COLOR_BLUE)Validation:$(COLOR_RESET)"
	@echo "    make validate-parens   - Check for unbalanced parentheses in modules"
	@echo "    make validate-modules  - Load all modules to verify they compile"
	@echo "    make compile           - Byte-compile all module files"
	@echo "    make lint              - Run all linters (checkdoc, package-lint, elisp-lint)"
	@echo ""
	@echo "  $(COLOR_BLUE)Utilities:$(COLOR_RESET)"
	@echo "    make profile           - Profile Emacs startup performance"
	@echo "    make clean             - Remove test artifacts and compiled files"
	@echo "    make clean-compiled    - Remove .elc/.eln files only"
	@echo "    make clean-tests       - Remove test artifacts only"
	@echo "    make reset             - Reset to first launch (DESTRUCTIVE!)"
	@echo ""
	@echo "Examples:"
	@echo "  make test-file FILE=test-custom-buffer-file-copy-whole-buffer.el"
	@echo "  make test-name TEST=test-custom-buffer-file-copy-*"
	@echo "  make EMACS=emacs29 test   # Use specific Emacs version"

# ============================================================================
# Testing Targets
# ============================================================================

test: test-all

test-all:
	@echo "$(COLOR_BLUE)Running all tests ($(words $(ALL_TESTS)) files)...$(COLOR_RESET)"
	@$(MAKE) test-unit
	@if [ $(words $(INTEGRATION_TESTS)) -gt 0 ]; then \
		$(MAKE) test-integration; \
	fi
	@echo "$(COLOR_GREEN)✓ All tests complete$(COLOR_RESET)"

test-unit:
	@echo "$(COLOR_BLUE)Running unit tests ($(words $(UNIT_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All unit tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed unit test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

test-integration:
	@if [ $(words $(INTEGRATION_TESTS)) -eq 0 ]; then \
		echo "$(COLOR_YELLOW)No integration tests found$(COLOR_RESET)"; \
		exit 0; \
	fi
	@echo "$(COLOR_BLUE)Running integration tests ($(words $(INTEGRATION_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(INTEGRATION_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All integration tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed integration test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

test-file:
ifndef FILE
	@echo "$(COLOR_RED)Error: FILE parameter required$(COLOR_RESET)"
	@echo "Usage: make test-file FILE=test-custom-buffer-file-copy-whole-buffer.el"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests in $(FILE)...$(COLOR_RESET)"
	@$(EMACS_TEST) -l ert -l $(TEST_DIR)/$(FILE) -f ert-run-tests-batch-and-exit
	@echo "$(COLOR_GREEN)✓ Tests in $(FILE) complete$(COLOR_RESET)"

test-name:
ifndef TEST
	@echo "$(COLOR_RED)Error: TEST parameter required$(COLOR_RESET)"
	@echo "Usage: make test-name TEST=test-custom-buffer-file-copy-*"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests matching pattern: $(TEST)...$(COLOR_RESET)"
	@$(EMACS_TEST) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(test)) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "$(COLOR_GREEN)✓ Tests matching '$(TEST)' complete$(COLOR_RESET)"

# ============================================================================
# Validation Targets
# ============================================================================

validate-parens:
	@echo "$(COLOR_BLUE)Checking for unbalanced parentheses in modules...$(COLOR_RESET)"
	@failed=0; \
	for file in $(MODULE_FILES); do \
		echo "  Checking $$file..."; \
		$(EMACS_BATCH) --eval "(condition-case err \
			(progn \
				(find-file \"$$file\") \
				(check-parens) \
				(kill-emacs 0)) \
			(error (progn \
				(message \"ERROR: %s\" err) \
				(kill-emacs 1))))" 2>&1 > /dev/null || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All modules have balanced parentheses$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed module(s) have unbalanced parentheses$(COLOR_RESET)"; \
		exit 1; \
	fi

validate-modules:
	@echo "$(COLOR_BLUE)Loading all modules to verify compilation...$(COLOR_RESET)"
	@failed=0; \
	for file in $(MODULE_FILES); do \
		echo "  Loading $$file..."; \
		$(EMACS_BATCH) -L $(MODULE_DIR) \
			--eval "(condition-case err \
				(progn \
					(load-file \"$$file\") \
					(message \"OK: %s\" \"$$file\")) \
				(error (progn \
					(message \"ERROR loading %s: %s\" \"$$file\" err) \
					(kill-emacs 1))))" || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All modules loaded successfully$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed module(s) failed to load$(COLOR_RESET)"; \
		exit 1; \
	fi

compile:
	@echo "$(COLOR_BLUE)Byte-compiling all modules...$(COLOR_RESET)"
	@$(EMACS_BATCH) -L $(MODULE_DIR) \
		--eval "(progn \
			(setq byte-compile-error-on-warn nil) \
			(batch-byte-compile))" $(MODULE_FILES)
	@echo "$(COLOR_GREEN)✓ Compilation complete$(COLOR_RESET)"

lint:
	@echo "$(COLOR_BLUE)Running linters on all modules...$(COLOR_RESET)"
	@echo "$(COLOR_YELLOW)Note: checkdoc, package-lint, and elisp-lint must be installed$(COLOR_RESET)"
	@failed=0; \
	for file in $(MODULE_FILES); do \
		echo "  Linting $$file..."; \
		$(EMACS_BATCH) -L $(MODULE_DIR) \
			--eval "(progn \
				(require 'checkdoc nil t) \
				(require 'package-lint nil t) \
				(require 'elisp-lint nil t) \
				(find-file \"$$file\") \
				(when (featurep 'checkdoc) \
					(checkdoc-current-buffer t)) \
				(when (featurep 'package-lint) \
					(package-lint-current-buffer)) \
				(when (featurep 'elisp-lint) \
					(elisp-lint-file \"$$file\")))" || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All linting checks passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_YELLOW)⚠ $$failed module(s) have linting issues$(COLOR_RESET)"; \
	fi

# ============================================================================
# Utility Targets
# ============================================================================

profile:
	@echo "$(COLOR_BLUE)Profiling Emacs startup...$(COLOR_RESET)"
	@if [ -f "$(EMACS_HOME)/early-init.el" ]; then \
		$(EMACS) -Q --load "$(EMACS_HOME)/custom/profile-dotemacs.el" \
			--eval "(progn (load-file \"$(EMACS_HOME)/early-init.el\") (profile-dotemacs))"; \
	else \
		echo "$(COLOR_YELLOW)No early-init.el found. Profiling init.el only.$(COLOR_RESET)"; \
		$(EMACS) -Q --load "$(EMACS_HOME)/custom/profile-dotemacs.el" \
			--eval "(profile-dotemacs)"; \
	fi

clean: clean-tests clean-compiled
	@echo "$(COLOR_GREEN)✓ Clean complete$(COLOR_RESET)"

clean-compiled:
	@echo "$(COLOR_BLUE)Removing compiled files (.elc, .eln)...$(COLOR_RESET)"
	@find $(EMACS_HOME) -type f \( -name "*.eln" -o -name "*.elc" \) -delete
	@echo "$(COLOR_GREEN)✓ Compiled files removed$(COLOR_RESET)"

clean-tests:
	@echo "$(COLOR_BLUE)Removing test artifacts...$(COLOR_RESET)"
	@rm -rf $(HOME)/.temp-emacs-tests
	@echo "$(COLOR_GREEN)✓ Test artifacts removed$(COLOR_RESET)"

reset:
	@echo "$(COLOR_RED)⚠ DESTRUCTIVE: Resetting to first launch...$(COLOR_RESET)"
	@rm -rf $(HOME)/.cache/org-persist/
	@rm -rf $(EMACS_HOME)/.cache/
	@rm -rf $(EMACS_HOME)/.elfeed-db/
	@rm -rf $(EMACS_HOME)/auto-save-list/
	@rm -rf $(EMACS_HOME)/backups/
	@rm -rf $(EMACS_HOME)/crossword/
	@rm -rf $(EMACS_HOME)/dirvish/
	@rm -rf $(EMACS_HOME)/eln-cache/
	@rm -rf $(EMACS_HOME)/elpa/
	@rm -rf $(EMACS_HOME)/emms/
	@rm -rf $(EMACS_HOME)/emojis/
	@rm -rf $(EMACS_HOME)/erc/
	@rm -rf $(EMACS_HOME)/eshell/
	@rm -rf $(EMACS_HOME)/multisession
	@rm -rf $(EMACS_HOME)/nov-places/
	@rm -rf $(EMACS_HOME)/persist/
	@rm -rf $(EMACS_HOME)/quelpa/
	@rm -rf $(EMACS_HOME)/request/
	@rm -rf $(EMACS_HOME)/tramp-auto-save/
	@rm -rf $(EMACS_HOME)/transient/
	@rm -rf $(EMACS_HOME)/tree-sitter/
	@rm -rf $(EMACS_HOME)/url/
	@rm -rf $(EMACS_HOME)/var
	@rm -f $(EMACS_HOME)/.elfeed-db
	@rm -f $(EMACS_HOME)/.emacs-history
	@rm -f $(EMACS_HOME)/.lsp-session*
	@rm -f $(EMACS_HOME)/.org-generic-id-locations
	@rm -f $(EMACS_HOME)/.org-id-locations
	@rm -f $(EMACS_HOME)/.pdf-view-restore
	@rm -f $(EMACS_HOME)/.scratch
	@rm -f $(EMACS_HOME)/forge-database.sqlite
	@rm -f $(EMACS_HOME)/history
	@rm -f $(EMACS_HOME)/nov-places
	@rm -f $(EMACS_HOME)/org-roam.db
	@rm -f $(EMACS_HOME)/pomm
	@rm -f $(EMACS_HOME)/projectile-bookmarks.eld
	@rm -f $(EMACS_HOME)/projects
	@rm -f $(EMACS_HOME)/recentf
	@rm -f $(EMACS_HOME)/tramp-connection-history
	@rm -f $(HOME)/sync/org/emacs-theme.persist
	@find $(EMACS_HOME) -name "*.eln" -type f -delete
	@find $(EMACS_HOME) -name "*.elc" -type f -delete
	@echo "$(COLOR_GREEN)✓ Reset complete$(COLOR_RESET)"
