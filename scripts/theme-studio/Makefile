# Makefile for the theme-studio tool — a self-contained Python + JS subproject.
# Its toolchain (python3, node, uvx, headless Chrome) is independent of the repo
# root's Elisp/ERT world, so the build logic lives here with the code. The root
# Makefile delegates: `make theme-studio-test` and `make theme-studio-coverage`
# call `make -C scripts/theme-studio ...`.
#
# Recipes run in this directory, so the relative paths below resolve whether you
# `cd` here or invoke via the root's `-C` delegation.

# Absolute path to this directory (for `open`, which hands Chrome a file path).
HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# Optional palette seed for `gen` / `open`: make gen SEED=dupre.json
SEED ?=

.PHONY: help test check coverage gen open

.DEFAULT_GOAL := help

help:
	@echo "theme-studio targets:"
	@echo "  make test                - Full suite: Python + Node + browser hash gates"
	@echo "  make check               - Fast gate: regenerate + Python + Node (no browser)"
	@echo "  make coverage            - JS (node) + generate.py (uvx coverage) numbers"
	@echo "  make gen [SEED=x.json]   - Regenerate theme-studio.html (optionally from a seed)"
	@echo "  make open [SEED=x.json]  - Regenerate and open the page in Chrome"

test:
	@./run-tests.sh

check:
	@./run-tests.sh --no-browser

coverage:
	@echo "== JS coverage (node --experimental-test-coverage) =="
	@node --test --experimental-test-coverage ./*.mjs 2>/dev/null \
		| sed -n '/start of coverage report/,/end of coverage report/p'
	@echo ""
	@echo "== generate.py coverage =="
	@if command -v uvx >/dev/null 2>&1; then \
		uvx coverage run --include='generate.py' -m unittest test_generate >/dev/null 2>&1; \
		uvx coverage report -m; \
		uvx coverage erase >/dev/null 2>&1; \
	else \
		echo "uvx not found — skipping generate.py line coverage"; \
		echo "($$(grep -c 'def test_' test_generate.py) test_generate.py tests exist)"; \
	fi

gen:
	@THEME_STUDIO_SEED="$(SEED)" python3 generate.py

open: gen
	@c=""; for b in google-chrome-stable google-chrome chromium chromium-browser; do \
		command -v $$b >/dev/null 2>&1 && { c=$$b; break; }; \
	done; \
	if [ -n "$$c" ]; then \
		"$$c" "$(HERE)theme-studio.html" >/dev/null 2>&1 & \
		echo "opened theme-studio.html in $$c"; \
	else \
		echo "no Chromium-family browser found"; exit 1; \
	fi
