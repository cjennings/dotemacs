# Makefile for Emacs configuration
#
# Usage:
#   make help              - Show this help message
#   make test              - Run all tests
#   make test-unit         - Run unit tests only
#   make test-file FILE=test-foo.el  - Run specific test file
#   make test-name TEST=test-foo-*   - Run tests matching pattern
#   make test-bash         - Run the bats shell-script tests
#   make benchmark         - Run performance benchmarks (:perf-tagged tests)
#   make coverage          - Generate simplecov coverage report and summary
#   make coverage-summary  - Summarize existing coverage by module
#   make coverage-clean    - Remove coverage report file
#   make validate-parens   - Check for unbalanced parentheses
#   make validate-modules  - Load all modules to verify they compile
#   make compile           - Byte-compile all modules
#   make compile-file FILE=  - Byte-compile one file with the project load path
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
OUT ?= themes

# Test files
UNIT_TESTS = $(filter-out $(TEST_DIR)/test-integration-%.el, $(wildcard $(TEST_DIR)/test-*.el))
INTEGRATION_TESTS = $(wildcard $(TEST_DIR)/test-integration-%.el)
ALL_TESTS = $(UNIT_TESTS) $(INTEGRATION_TESTS)
BASH_TESTS = $(wildcard $(TEST_DIR)/*.bats)

# Module files
MODULE_FILES = $(wildcard $(MODULE_DIR)/*.el)

# Emacs batch flags
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp --eval '(setq load-prefer-newer t)'
EMACS_TEST = $(EMACS_BATCH) -L $(TEST_DIR) -L $(MODULE_DIR)

# No colors - using plain text symbols instead

.PHONY: help targets test test-all test-unit test-integration test-file test-name \
        test-bash theme-studio-test theme-studio-check theme-studio-coverage theme-studio-gen theme-studio-open theme-studio-theme theme-studio-theme-load theme-studio-theme-reload \
        benchmark coverage coverage-summary coverage-clean \
        validate-parens validate-modules compile compile-file lint profile \
        clean clean-compiled clean-tests reset

# Alias for help
targets: help

# Default target
.DEFAULT_GOAL := help

help:
	@echo "Emacs Configuration Targets:"
	@echo ""
	@echo "  Testing:"
	@echo "    make test              - Run all tests ($(words $(ALL_TESTS)) files)"
	@echo "    make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "    make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "    make test-file FILE=<filename>  - Run specific test file"
	@echo "    make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo "    make test-bash         - Run the bats shell-script tests ($(words $(BASH_TESTS)) files)"
	@echo "    make benchmark         - Run performance benchmarks (:perf-tagged)"
	@echo ""
	@echo "  theme-studio (delegates to scripts/theme-studio/Makefile):"
	@echo "    make theme-studio-test     - Full suite (Python + Node + browser gates)"
	@echo "    make theme-studio-check    - Fast gate (regenerate + Python + Node, no browser)"
	@echo "    make theme-studio-coverage - JS + generate.py coverage numbers"
	@echo "    make theme-studio-gen      - Regenerate theme-studio.html (SEED=x.json optional)"
	@echo "    make theme-studio-open     - Regenerate and open the page in Chrome"
	@echo "    make theme-studio-theme JSON=/path/theme.json - Convert JSON export to themes/<name>-theme.el"
	@echo "    make theme-studio-theme-load THEME=name       - Disable all custom themes, then load THEME"
	@echo "    make theme-studio-theme-reload JSON=/path.json - Convert JSON, then cleanly reload its theme"
	@echo ""
	@echo "  Coverage:"
	@echo "    make coverage          - Generate simplecov JSON and summarize modules"
	@echo "    make coverage-summary  - Summarize existing coverage by module"
	@echo "    make coverage-clean    - Delete the coverage report file"
	@echo ""
	@echo "  Validation:"
	@echo "    make validate-parens   - Check for unbalanced parentheses in modules"
	@echo "    make validate-modules  - Load all modules to verify they compile"
	@echo "    make compile           - Byte-compile all module files"
	@echo "    make compile-file FILE=path/to.el - Byte-compile one file with the project load path"
	@echo "    make lint              - Run all linters (checkdoc, package-lint, elisp-lint)"
	@echo ""
	@echo "  Utilities:"
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
	@echo "[i] Running all tests ($(words $(ALL_TESTS)) Elisp files)..."
	@$(MAKE) test-unit
	@if [ $(words $(INTEGRATION_TESTS)) -gt 0 ]; then \
		$(MAKE) test-integration; \
	fi
	@if [ $(words $(BASH_TESTS)) -gt 0 ] && command -v bats >/dev/null 2>&1; then \
		$(MAKE) test-bash; \
	fi
	@echo "✓ All tests complete"

test-bash:
	@if [ $(words $(BASH_TESTS)) -eq 0 ]; then \
		echo "No bats tests found"; \
		exit 0; \
	fi
	@if ! command -v bats >/dev/null 2>&1; then \
		echo "[!] bats not installed — skipping shell-script tests"; \
		exit 0; \
	fi
	@echo "[i] Running bats shell-script tests ($(words $(BASH_TESTS)) files)..."
	@bats $(BASH_TESTS)

theme-studio-test:
	@$(MAKE) -C scripts/theme-studio test

theme-studio-check:
	@$(MAKE) -C scripts/theme-studio check

theme-studio-coverage:
	@$(MAKE) -C scripts/theme-studio coverage

theme-studio-gen:
	@$(MAKE) -C scripts/theme-studio gen SEED='$(SEED)'

theme-studio-open:
	@$(MAKE) -C scripts/theme-studio open SEED='$(SEED)'

theme-studio-theme:
ifndef JSON
	@echo "Error: JSON parameter required"
	@echo "Usage: make theme-studio-theme JSON=/path/to/theme.json [OUT=themes]"
	@exit 1
endif
	@$(MAKE) -C scripts/theme-studio theme JSON='$(abspath $(JSON))' OUT='$(abspath $(OUT))'

theme-studio-theme-load:
ifndef THEME
	@echo "Error: THEME parameter required"
	@echo "Usage: make theme-studio-theme-load THEME=theme [OUT=themes]"
	@exit 1
endif
	@$(MAKE) -C scripts/theme-studio theme-load THEME='$(THEME)' OUT='$(abspath $(OUT))'

theme-studio-theme-reload:
ifndef JSON
	@echo "Error: JSON parameter required"
	@echo "Usage: make theme-studio-theme-reload JSON=/path/to/theme.json [OUT=themes] [THEME=name]"
	@exit 1
endif
	@$(MAKE) -C scripts/theme-studio theme-reload JSON='$(abspath $(JSON))' OUT='$(abspath $(OUT))' THEME='$(THEME)'

BANNER = ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# Run the .el test files in $(1), each in its own Emacs, with :slow and :perf
# tagged tests skipped.  $(2) is the lowercase phrase for status lines
# ("unit tests"); $(3) the uppercase phrase for the banner ("UNIT TESTS").
# The whole body is one shell command, so callers invoke it as a single
# recipe line: `@$(call run-el-tests,...)'.
define run-el-tests
echo "[i] Running $(2) ($(words $(1)) files)..."; \
echo ""; \
failed=0; \
failed_files=""; \
for test in $(1); do \
	test_name=$$(basename $$test); \
	printf "  Testing %-60s " "$$test_name..."; \
	output=$$($(EMACS_TEST) -l ert -l $$test --eval "(ert-run-tests-batch-and-exit '(not (or (tag :slow) (tag :perf))))" 2>&1); \
	result=$$?; \
	if [ $$result -eq 0 ]; then \
		pass_count=$$(echo "$$output" | grep -oP "Ran \K\d+" | head -1); \
		echo "✓ ($$pass_count tests)"; \
	else \
		echo "✗ FAILED"; \
		failed=$$((failed + 1)); \
		failed_files="$$failed_files$$test_name "; \
		echo "$$output" | grep -E "FAILED|unexpected|Error" > /tmp/test-failure-$$test_name.log; \
	fi; \
done; \
echo ""; \
if [ $$failed -eq 0 ]; then \
	echo "$(BANNER)"; \
	echo "✓ ALL $(3) PASSED"; \
	echo "$(BANNER)"; \
else \
	echo "$(BANNER)"; \
	echo "✗ FAILURES DETECTED: $$failed test file(s) failed"; \
	echo "$(BANNER)"; \
	echo ""; \
	echo "Failed test files:"; \
	for file in $$failed_files; do \
		echo "  • $$file"; \
		if [ -f /tmp/test-failure-$$file.log ]; then \
			echo "    Errors:"; \
			sed 's/^/      /' /tmp/test-failure-$$file.log; \
			rm /tmp/test-failure-$$file.log; \
		fi; \
	done; \
	echo ""; \
	echo "Run individual failing tests with:"; \
	for file in $$failed_files; do \
		echo "  make test-file FILE=$$file"; \
	done; \
	echo ""; \
	exit 1; \
fi
endef

test-unit:
	@$(call run-el-tests,$(UNIT_TESTS),unit tests,UNIT TESTS)

test-integration:
	@if [ $(words $(INTEGRATION_TESTS)) -eq 0 ]; then \
		echo "No integration tests found"; \
		exit 0; \
	fi
	@$(call run-el-tests,$(INTEGRATION_TESTS),integration tests,INTEGRATION TESTS)

test-file:
ifndef FILE
	@echo "Error: FILE parameter required"
	@echo "Usage: make test-file FILE=test-custom-buffer-file-copy-whole-buffer.el"
	@exit 1
endif
	@echo "Running tests in $(FILE)..."
	@$(EMACS_TEST) -l ert -l $(TEST_DIR)/$(FILE) --eval "(ert-run-tests-batch-and-exit '(not (tag :slow)))"
	@echo "✓ Tests in $(FILE) complete"

test-name:
ifndef TEST
	@echo "Error: TEST parameter required"
	@echo "Usage: make test-name TEST=test-custom-buffer-file-copy-*"
	@exit 1
endif
	@echo "Running tests matching pattern: $(TEST)..."
	@$(EMACS_TEST) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(abspath $(test))) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "✓ Tests matching '$(TEST)' complete"

# Performance benchmarks for lorem-optimum's Markov chain.  These are tagged
# `:perf' so `make test', `make coverage', and the editor test hook skip them;
# this target is the way to run them on purpose.
BENCHMARK_TESTS = $(TEST_DIR)/test-lorem-optimum-benchmark.el

benchmark:
	@echo "[i] Running performance benchmarks..."
	@$(EMACS_TEST) -l ert $(foreach test,$(BENCHMARK_TESTS),-l $(test)) \
		--eval "(ert-run-tests-batch-and-exit '(tag :perf))"
	@echo "✓ Benchmarks complete"

# ============================================================================
# Coverage Targets
# ============================================================================

COVERAGE_DIR = .coverage
COVERAGE_FILE = $(COVERAGE_DIR)/simplecov.json

# test-all-comp-errors byte-compiles every module, which fails on sources
# undercover has instrumented, so it can't run under `make coverage'.  (The
# :perf-tagged benchmarks are skipped by the tag filter above, not listed here.)
COVERAGE_EXCLUDE = \
	$(TEST_DIR)/test-all-comp-errors.el
COVERAGE_TESTS = $(filter-out $(COVERAGE_EXCLUDE),$(UNIT_TESTS))

coverage: coverage-clean $(COVERAGE_DIR)
	@echo "[i] Deleting compiled coverage targets so undercover can instrument sources..."
	@rm -f $(MODULE_DIR)/*.elc gptel-tools/*.elc
	@echo "[i] Running coverage across $(words $(COVERAGE_TESTS)) test files..."
	@echo "    (this is slower than 'make test' — each file runs in its own Emacs)"
	@echo "    excluded from coverage: $(notdir $(COVERAGE_EXCLUDE))"
	@echo ""
	@failed=0; \
	failed_files=""; \
	for test in $(COVERAGE_TESTS); do \
		test_name=$$(basename $$test); \
		printf "  Coverage: %-58s " "$$test_name..."; \
		output=$$($(EMACS_TEST) -l $(TEST_DIR)/run-coverage-file.el -l $$test --eval "(ert-run-tests-batch-and-exit '(not (or (tag :slow) (tag :perf))))" 2>&1); \
		result=$$?; \
		if [ $$result -eq 0 ]; then \
			echo "✓"; \
		else \
			echo "✗"; \
			failed=$$((failed + 1)); \
			failed_files="$$failed_files $$test_name"; \
		fi; \
	done; \
	echo ""; \
	if [ $$failed -gt 0 ]; then \
		echo "[!] $$failed test file(s) failed during coverage run:"; \
		echo "$$failed_files" | tr ' ' '\n' | grep -v '^$$' | sed 's/^/    /'; \
		exit 1; \
	fi
	@if [ -f $(COVERAGE_FILE) ]; then \
		echo "✓ Coverage report: $(COVERAGE_FILE) ($$(du -h $(COVERAGE_FILE) | cut -f1))"; \
	else \
		echo "[!] No coverage file produced; check that undercover is installed"; \
		exit 1; \
	fi
	@$(MAKE) coverage-summary

coverage-summary:
	@if [ ! -f $(COVERAGE_FILE) ]; then \
		echo "[!] No coverage file found at $(COVERAGE_FILE). Run 'make coverage' first."; \
		exit 1; \
	fi
	@$(EMACS_BATCH) -L $(MODULE_DIR) -L scripts -l coverage-summary \
		--eval '(cj/coverage-print-module-summary "$(COVERAGE_FILE)" "$(MODULE_DIR)" "$(CURDIR)")'

coverage-clean:
	@rm -f $(COVERAGE_FILE)

$(COVERAGE_DIR):
	@mkdir -p $(COVERAGE_DIR)

# ============================================================================
# Validation Targets
# ============================================================================

validate-parens:
	@echo "Checking for unbalanced parentheses in modules..."
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
		echo "✓ All modules have balanced parentheses"; \
	else \
		echo "✗ $$failed module(s) have unbalanced parentheses"; \
		exit 1; \
	fi

validate-modules:
	@echo "Loading all modules to verify compilation..."
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
		echo "✓ All modules loaded successfully"; \
	else \
		echo "✗ $$failed module(s) failed to load"; \
		exit 1; \
	fi

compile:
	@echo "Byte-compiling all modules..."
	@$(EMACS_BATCH) -L $(MODULE_DIR) -L themes \
		--eval "(progn \
			(package-initialize) \
			(setq byte-compile-error-on-warn nil) \
			(batch-byte-compile))" $(MODULE_FILES)
	@echo "✓ Compilation complete"

# Byte-compile a single file with the project load path (modules, themes,
# tests) so local compile-time requires resolve.  Bare `emacs -Q --batch
# byte-compile-file ...' fails on those requires (e.g. dupre-palette,
# undead-buffers); use this instead.  Example:
#   make compile-file FILE=modules/dashboard-config.el
#   make compile-file FILE=themes/dupre-faces.el
compile-file:
	@test -n "$(FILE)" || { echo "Usage: make compile-file FILE=path/to/file.el"; exit 1; }
	@$(EMACS_BATCH) -L $(MODULE_DIR) -L themes -L $(TEST_DIR) \
		--eval "(package-initialize)" \
		--eval "(or (byte-compile-file \"$(FILE)\") (kill-emacs 1))"
	@echo "✓ Compiled $(FILE)"

lint:
	@echo "Running linters on all modules..."
	@echo "Note: checkdoc, package-lint, and elisp-lint must be installed"
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
		echo "✓ All linting checks passed"; \
	else \
		echo "⚠ $$failed module(s) have linting issues"; \
	fi

# ============================================================================
# Utility Targets
# ============================================================================

profile:
	@echo "Profiling Emacs startup..."
	@if [ -f "$(EMACS_HOME)/early-init.el" ]; then \
		$(EMACS) -Q --load "$(EMACS_HOME)/custom/profile-dotemacs.el" \
			--eval "(progn (load-file \"$(EMACS_HOME)/early-init.el\") (profile-dotemacs))"; \
	else \
		echo "No early-init.el found. Profiling init.el only."; \
		$(EMACS) -Q --load "$(EMACS_HOME)/custom/profile-dotemacs.el" \
			--eval "(profile-dotemacs)"; \
	fi

clean: clean-tests clean-compiled
	@echo "✓ Clean complete"

clean-compiled:
	@echo "Removing compiled files (.elc, .eln)..."
	@find $(EMACS_HOME) -type f \( -name "*.eln" -o -name "*.elc" \) -delete
	@echo "✓ Compiled files removed"

clean-tests:
	@echo "Removing test artifacts..."
	@test_dir="$${CJ_EMACS_TEST_DIR:-$${TMPDIR:-/tmp}/cj-emacs-tests}"; \
	 rm -rf "$$test_dir" "$${TMPDIR:-/tmp}"/cj-emacs-tests-* "$(HOME)/.temp-emacs-tests"
	@echo "✓ Test artifacts removed"

reset:
	@echo "⚠ DESTRUCTIVE: Resetting to first launch..."
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
	@echo "✓ Reset complete"
