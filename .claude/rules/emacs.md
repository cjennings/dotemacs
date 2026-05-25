# Working With Craig's Running Emacs

Applies to: `**/*.el` (and any task that edits Craig's Emacs configuration)

Craig works inside Emacs nearly constantly, with a long-running `emacs --daemon` and `emacsclient` frames. When you edit an Emacs module he's using, **do not make him quit, relaunch, and re-open all his files.** Push the change into the running daemon instead. He explicitly called the restart-and-reload-everything dance out as the thing this avoids.

## Live-reload a module

After editing a module, reload it into the running daemon:

    emacsclient -e '(load "/home/cjennings/.emacs.d/modules/foo.el")'

This re-evaluates the file and redefines its `defun`s live. For straight function redefinitions it is immediate and clean — the edited command is active on the next keypress, no restart.

## Caveats — when a plain reload isn't enough

- **`defvar` defaults don't re-apply.** `defvar` only assigns when the variable is unbound, so reloading the file won't change an already-set default. `setq` the value explicitly, or `makunbound` it then reload.
- **`use-package` `:config` / `:init` re-run on a full file `load`.** That re-adds hooks, re-binds keys, re-runs setup — functionally fine, but it stacks state and isn't pristine. For an edit inside a `:config` block, prefer eval-ing just the changed form, or accept the restacking, or restart for a clean state.
- **Faces and themes need re-applying.** Editing a theme/face file does nothing until the theme is re-applied: `(load-theme 'THEME t)`.
- **Baked or rendered state must be regenerated.** Values computed at load time (e.g. a list built from `nerd-icons-*` calls) and already-drawn buffers (e.g. `*dashboard*`) do not update from a var reload alone — regenerate them (e.g. `dashboard-refresh-buffer`). This is the stale-buffer trap: the variable looks correct but the visible buffer is old. It once made a screenshot look right when the live buffer was still wrong.

## The reload-and-verify loop (default for visible Emacs changes)

1. Edit the module.
2. Reload into the daemon (`emacsclient -e '(load ...)'`), plus re-apply the theme and/or regenerate the affected buffer where the caveats above apply.
3. Verify: for visual changes, screenshot and read it (the `screenshot.py` tool under `.ai/scripts/` can capture an app off-screen on a headless output); for behavior, eval or exercise it.

This replaces the quit → relaunch → re-find-and-load-files cycle for most edits. A real restart stays the gold standard for a guaranteed-clean state — anything touching `:config`, load order, or when in doubt.
