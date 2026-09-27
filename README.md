# My Emacs config

This repository contains my Emacs configuration files. Tested on Emacs 31. The config's own code needs at least 29.1 (tree-sitter modes, `setopt`, `keymap-set`, `pos-eol`).

# Loading order

`.emacs` loads, in order:

1. `.emacs_lisp/config/init-pre.el`, if it exists, then calls `config-init-pre` if that file defines it.
1. `init-custom.el`, then the custom file `.emacs_lisp/custom.el`, if it exists. Because the custom file loads before the other modules, `:custom` values in `use-package` blocks override values saved through Customize.
1. Each module in `dn-modules`, in order. `init-programming.el` then loads every language module in `programming/` except those listed in `dn-disabled-languages`.
1. `.emacs_lisp/config/init-post.el`, if it exists, then `config-init-post` if defined.

Modules are loaded with `config-require`, which looks in `.emacs_lisp/config/` before `.emacs_lisp/`. A file there with the same name as a tracked module replaces it.

# Per-host customisation

`init-pre.el`, `init-post.el` and `custom.el` are not under version control. `init-pre.el` runs before any module loads, so it can change what gets loaded:

```elisp
(setq dn-modules (remq 'init-llm dn-modules))        ; skip a module
(setq dn-disabled-languages '(matlab gnuplot rest))  ; skip language modules
```

`dn-disabled-languages` takes file base names from `programming/`, as symbols.

# Layout

| Path | Contents |
|---|---|
| `.emacs` | Entry point |
| `.emacs_lisp/init-*.el` | One module per concern, listed in `dn-modules` |
| `.emacs_lisp/programming/` | Language modules; `foo.el` must provide `init-prog-foo` |
| `.emacs_lisp/config/` | Paths (`variables.el`), loader helpers (`functions.el`) and the untracked per-host files |
| `.emacs_lisp/vendor/` | Third-party libraries |
| `.emacs_lisp/lib/` | Own libraries |
| `.emacs_lisp/patches/` | Code that modifies a package after it loads |
| `.emacs_lisp/packages/` | Own packages (ROS 2 modes) |
| `.emacs_lisp/yas-lib/` | Helpers for snippets; `foo.el` must provide `yas-lib-foo` |
| `.emacs_lisp/snippets/` | yasnippet snippets |
| `.emacs_lisp/abbrev_*` | Abbrev tables |
| `.emacs_lisp/docsets/` | Dash docsets installed by `dn-dash-docs-install` |

# Checking the config

`make check` loads the whole config in batch mode and runs `test/smoke.el`. It exits non-zero on a load error, a `use-package` warning or a failed check.
