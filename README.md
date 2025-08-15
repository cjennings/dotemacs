# dotemacs

This repository contains Craig Jennings' modular Emacs configuration.  It is
mirrored at [git.cjennings.net](https://git.cjennings.net/dotemacs.git).

## Structure

* **early-init.el** – minimal bootstrap settings loaded before package
auto-initialisation.
* **init.el** – orchestrates configuration by loading modules.
* **modules/** – the bulk of the configuration, split into focused modules
  (Org, programming languages, UI, etc.).
* **custom/** and **assets/** – supporting files, personal data, and extra
  utilities.
* **themes/** – custom theme tweaks.
* **scripts/** – helper scripts for maintaining this setup.
* **tests/** – ERT tests for many helper functions.

## Highlights

* **Custom helpers** – over **33** `cj/` prefixed functions collected in
  `modules/custom-functions.el`, bound under a personal `C-;` keymap for quick
action on text and files.
* **Flyspell & Abbrev** – `flyspell-and-abbrev.el` coordinates flyspell with
  abbrev so every correction can automatically become an expansion.
* **E‑book and PDF integration** – `calibredb` manages the library, `nov` reads
  EPUBs with custom rendering, and `pdf-tools` provides a feature rich PDF viewer
  (with quick hand‑off to external viewers when needed).
* **Org‑roam workflow** – daily journals and templated project/topic/recipe
  notes, helper commands to find nodes by tag, capture tasks straight into
  projects, and automatically copy completed TODOs to the day’s journal.
* **Org‑drill & capture** – flashcard sessions via `C-d` bindings and capture
  templates that pull content from the web, EPUBs, or PDFs for spaced
  repetition.
* **Org webpage clipper** – `cj/org-webpage-clipper` saves a rendered copy of
  the current page from EWW or W3M directly into an Org file.
* **Org‑agenda setup** – F8 opens a custom agenda showing high priority tasks,
  the schedule, and remaining work; additional commands build task lists from
  all agenda files or just the current buffer.
* **Dirvish** – replaces Dired with a modern file manager offering quick access
  shortcuts, external open commands, image conversion, and more.
* **Programmer tools** – relative line numbers, project management with
  Projectile, ripgrep searching, Yasnippet snippets, symbol overlays, rainbow
  colors, Tree‑sitter, LSP, and other language‑specific modules.

## Misc

The configuration is tested with ERT; see the `tests/` directory for examples of
how functions are exercised.

