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
OUT ?= ../../themes
EMACS ?= emacs
EMACSCLIENT ?= emacsclient

.PHONY: help test check check-generated coverage gen open theme theme-load theme-reload face-coverage-dump face-coverage face-coverage-diff

# Scratch path for the face-coverage Emacs data dump.
FACE_DUMP ?= /tmp/face-coverage-data.json

.DEFAULT_GOAL := help

help:
	@echo "theme-studio targets:"
	@echo "  make test                - Full suite: Python + Node + browser hash gates"
	@echo "  make check               - Fast gate: regenerate + Python + Node (no browser)"
	@echo "  make check-generated     - Verify committed theme-studio.html is current"
	@echo "  make coverage            - JS (node) + generate.py (uvx coverage) numbers"
	@echo "  make gen [SEED=x.json]   - Regenerate theme-studio.html (optionally from a seed)"
	@echo "  make open [SEED=x.json]  - Regenerate and open the page in Chrome"
	@echo "  make theme JSON=x.json   - Convert a Theme Studio JSON export to OUT/<name>-theme.el"
	@echo "  make theme-load THEME=x  - Disable all custom themes, then load THEME in current Emacs"
	@echo "  make theme-reload JSON=x - Convert JSON, then cleanly reload its theme in current Emacs"
	@echo "  make face-coverage       - Regenerate face-coverage.org from the live Emacs daemon"
	@echo "  make face-coverage-diff  - Show the coverage delta vs the committed face-coverage.org"

test:
	@./run-tests.sh

check:
	@./run-tests.sh --no-browser

check-generated:
	@tmp="$$(mktemp)"; \
	cp theme-studio.html "$$tmp"; \
	restore() { cp "$$tmp" theme-studio.html; rm -f "$$tmp"; }; \
	if ! python3 generate.py >/dev/null; then restore; exit 1; fi; \
	if cmp -s theme-studio.html "$$tmp"; then rm -f "$$tmp"; echo "theme-studio.html is current"; \
	else restore; echo "theme-studio.html is stale; run make gen and commit it" >&2; exit 1; fi

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

theme:
ifndef JSON
	@echo "Error: JSON parameter required"
	@echo "Usage: make theme JSON=/path/to/theme.json [OUT=../../themes]"
	@exit 1
endif
	@$(EMACS) --batch -l build-theme.el --eval '(princ (concat "wrote " (build-theme/convert-file "$(JSON)" "$(OUT)") "\n"))'

theme-load:
ifndef THEME
	@echo "Error: THEME parameter required"
	@echo "Usage: make theme-load THEME=theme [OUT=../../themes]"
	@exit 1
endif
	@$(EMACSCLIENT) -e "(progn (add-to-list 'custom-theme-load-path \"$(abspath $(OUT))\") (mapc #'disable-theme (copy-sequence custom-enabled-themes)) (load-theme '$(THEME) t) custom-enabled-themes)"

theme-reload:
ifndef JSON
	@echo "Error: JSON parameter required"
	@echo "Usage: make theme-reload JSON=/path/to/theme.json [OUT=../../themes] [THEME=name]"
	@exit 1
endif
	@$(MAKE) theme JSON='$(JSON)' OUT='$(OUT)' EMACS='$(EMACS)'
	@theme_name='$(THEME)'; \
	if [ -z "$$theme_name" ]; then theme_name="$$(basename '$(JSON)' .json)"; fi; \
	$(MAKE) theme-load THEME="$$theme_name" OUT='$(OUT)' EMACSCLIENT='$(EMACSCLIENT)'

# Dump face/group/package data from the running daemon (falls back to a batch
# Emacs that loads the full init when no daemon is reachable).
face-coverage-dump:
	@$(EMACSCLIENT) -e '(progn (load "$(HERE)face-coverage-dump.el") (face-coverage-dump "$(FACE_DUMP)"))' >/dev/null 2>&1 \
		|| $(EMACS) --batch -l "$$HOME/.emacs.d/init.el" -l "$(HERE)face-coverage-dump.el" \
			--eval '(face-coverage-dump "$(FACE_DUMP)")'

face-coverage: face-coverage-dump
	@python3 face_coverage.py --data "$(FACE_DUMP)"

face-coverage-diff: face-coverage-dump
	@python3 face_coverage.py --data "$(FACE_DUMP)" --compare face-coverage.org
