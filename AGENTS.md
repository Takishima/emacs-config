# emacs-config

Layout, loading order and per-host files are in [`README.md`](README.md). This file carries only
what changes a decision while you edit files here.

## Put each fact where its reader looks

Default to no comment. Before writing one, ask in order and stop at the first answer:

1. **Has the fact a home?** A name, a docstring, a `defcustom`'s `:type` and docstring, the file's
   `;;; Commentary:`, the commit message, or `README.md`. Write it there. A docstring reaches
   `C-h f` and `C-h v`; a comment reaches only whoever opens the source.
2. **Would a reader who hasn't made your mistake need it?** Change history, "now uses X instead of
   Y", and the path that led to the fix belong in the commit message.
3. **Two lines, max.** Past that, change the code: a better name, a helper with a docstring, a
   `defcustom` instead of a magic value.

Go longer only where nothing else can hold the fact: a caller obligation, an absence a reader would
fill in wrong (why a package is *not* deferred, why a hook is *not* buffer-local), or the reason for
a suppression such as `with-no-warnings` or `(declare-function ...)`. Section banners are not
homes; `use-package` blocks already name their section.

Shortening a comment that failed 1 or 2 does not fix it. Delete it.

## Follow Emacs Lisp conventions

- Comment syntax per the manual: `;` after code on the same line, `;;` for a line in a body, `;;;`
  for file headers. Two spaces after a sentence, in comments and docstrings alike.
- Docstrings pass `checkdoc`: a first line that is a complete sentence, arguments in capitals,
  symbols in `` `quotes' ``.
- Own symbols take the `dn-` prefix; a library in `lib/` uses its file name as prefix
  (`dn-timesheet-`). Private helpers use a double dash (`dn-timesheet--day-heading`).
- Every file opens with `-*- lexical-binding: t -*-`. Modules and libraries end with their
  `provide`; the entry points and `test/` scripts are loaded with `load` and need none.
- A value someone may want to change per host is a `defcustom`, not a `defvar` or a literal.

## Keep the minimum Emacs version

The config's own code targets Emacs 29.1 (`README.md` says why). Prefer the built-in forms that
version has: `setopt` over `setq` for user options, `keymap-set` over `define-key`, `pos-eol` over
`line-end-position`. Anything newer needs a `fboundp` or version guard.

## Reuse before you write

`config/functions.el` holds the loader helpers and `lib/` the own libraries. Check both, and what
Emacs or an already-installed package provides, before adding a helper. A new package dependency
needs a reason a few lines of Lisp cannot cover.

## Scope and precedence

These rules govern code you add or rewrite. Editing a module does not oblige you to restyle it.
Where consistency would require a wider rewrite, report that and ask. An explicit instruction in the
task outranks this file.

## Check before committing

1. `nix run .#check` passes. It runs `make check` (the config loads without errors or `use-package`
   warnings), `make compile` (no byte-compile warning) and `make packages` with the packages this
   checkout declares and no tools on PATH. A plain `make` runs whatever `emacs` is installed, so its
   result depends on when you last switched and on what is on PATH. For Nix changes, run
   `nix flake check`, after a `git add` of new files.
2. Code compiles the same whether or not an optional tool is installed: keep `defun`s and
   `defcustom`s at top level, not inside `(when (executable-find ...))`, or use `:defines` and
   `:functions`.
3. Every comment you added survives the three questions above.
4. Behaviour a user sees (a command, a key, a `defcustom`) is documented in its docstring, and in
   `README.md` when it belongs to a documented feature such as the timesheet.
