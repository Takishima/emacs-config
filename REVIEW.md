# Emacs config review: cleanup and modularity

Scope: every tracked `.el` file, the entry point, README, snippets layout,
service file and repo metadata, originally as of `47ec94c`, re-assessed against main `7514b9b` after the
73 cleanup and restructuring commits that followed the first pass. Findings were
checked against a running Emacs 31.1 session with this config loaded (via
`emacsclient`). Items marked **verify** still need a manual repro. Each
numbered item now starts with its status: **Done** (fixed on main),
**Partly** (some of it landed) or **Open**.

Numbers that frame the rest of the review:

| Metric | Value |
|---|---|
| `use-package` declarations | 143 (was 155), 67 of 123 blocks with a lazy trigger |
| Own init modules (`init-*.el`) | 22 (after the split of `init-emacs.el` and `init-programming.el`) |
| Language modules (`programming/*.el`) | 22 (two disabled via `dn-disabled-languages`) |
| Largest own module | `init-completion.el`, 440 lines (`init-emacs.el` is gone) |
| Vendored third-party code | 4 files, ~950 lines, now under `vendor/` |
| Own libraries | 2, now under `lib/` |
| Commented-out `use-package` blocks | 0 (was 12) |

## Status after main `7514b9b`

Main now carries every migration phase from Part 2, including the
restructure. The entry points are `init.el` and `early-init.el` at the
repository root, modules are listed in `dn-modules`, languages are
discovered by `dn-load-directory` under an enforced naming rule, macOS code
lives in `init-os-darwin.el`, and third-party and own libraries sit in
`vendor/` and `lib/`. `make check` batch-loads the config through the real
entry points and fails on any `use-package` warning.

Of the 37 cleanup items:

| Status | Items |
|---|---|
| Done | 1, 3-7, 9-22, 24-30, 35-37 |
| Partly | 2, 8, 31, 32, 33 |
| Open | 23, 34 |

What is left is small: `(use-package use-package :straight t)` (23), the
host-specific `emacs.service` (34), a `:straight nil` on `savehist` (32),
about 25 unprefixed own functions (31), tabs in 7 own files (33), and the
ispell setup still nested inside `flycheck-aspell` (2). Part 2 below has
been rewritten as an assessment of the new layout, with the remaining
structural gaps and a short list of next steps.

---

## Part 1: Cleanup

Ordered by impact. Section A is things that are wrong today, B is dead or
duplicated code, C is consistency and hygiene.

### A. Bugs and latent breakage

1. **Done.** **`exec-path-from-shell` initializes twice on Linux GUI sessions.**
   `init-env.el` has one block gated on `window-system` being `mac`/`ns`/`x`
   and another gated on `system-type` being `gnu/linux`. Under X on Linux
   both match, so `exec-path-from-shell-initialize` runs twice and the
   second block's variable list wins. Merge into one block with a computed
   list.

2. **Partly.** The `--reverse` override is gone; the nesting under `flycheck-aspell` remains. **`init-ispell.el` overrides its own spell-checker arguments.** Line 100
   computes `ispell-extra-args` from `flyspell-detect-ispell-args`, then line
   127 unconditionally does `(setq-default ispell-extra-args '("--reverse"))`,
   discarding the language and camel-case flags (live default value:
   `("--reverse")`). Also the entire ispell setup, including the hunspell
   fallback branch, lives inside `flycheck-aspell`'s `:config`, so it is tied
   to that package loading rather than standing on its own.

3. **Done.** **`programming/python.el` provides the wrong feature.** It ends with
   `(provide 'init-prog-build-systems)`, copied from `build-systems.el`. It
   only loads today because the discovery loop in `init-programming.el`
   uses `intern-soft`, which falls back to `load` when the symbol is not
   yet interned. Any change that interns `init-prog-python` first (for
   example mentioning it in another file) would flip the loop to `require`
   and it would fail with "Required feature was not provided".

4. **Done** (the `string-as-unibyte` calls remain). **`programming/matlab.el` redefines the built-in `string-replace`**
   (line 57). Emacs 28+ ships `string-replace` with the same arity, so the
   redefinition currently works by luck. It is skipped via `skip.txt`, but
   it is a trap if the skip is ever removed. Same file uses the obsolete
   `string-as-unibyte`.

5. **Done.** **`init-emacs.el` `dn-cmd-after-saved-file` is broken logic.** It
   computes `command` then calls `(shell-command (cdr match))` ignoring it,
   `add-to-list` on a `let`-bound list never updates the executed command,
   and the warning prints `command`, which is `nil` on the failure path.
   `dn-script-on-save` is also empty, so the hook runs for nothing on every
   save. Delete or rewrite.

6. **Done.** **Undeclared runtime dependencies** that only work because some other
   package pulls them in transitively:
   - `s-trim` (s.el) in `dn-async-process`, `init-emacs.el:708`
   - `f-exists?` (f.el) in `init-custom-functions.el:106`
   - `htmlize-buffer`/`htmlize-region`, `init-emacs.el:666,681`, never
     installed anywhere
   - `treemacs--icon-size` read unconditionally in `dn-adjust-font-size`
     (`init-emacs.el:752`); void-variable error if treemacs is not loaded
   - `consult` itself has no `:straight t` (`init-emacs.el:410`); it is
     only installed because `consult-dir`/`consult-lsp` depend on it.
     `straight-use-package-by-default` is never set, so every block without
     `:straight` is silently "configure only".

7. **Done.** **`magit-popup` is obsolete and the one consumer is probably broken.**
   (**verify**) `python-pytest` switched to transient years ago, so
   `magit-define-popup-option 'python-pytest-dispatch …`
   (`python.el:244-250`) targets a transient prefix, not a popup variable.
   If it signals, `use-package` aborts the rest of that `:config`, and
   `python-pytest-close-buffer` (`C-x tk`) is never defined. Repro: open a
   Python buffer, press `C-x tk`, check `*Warnings*`. `magit-popup` is also
   listed in `emojify-inhibit-major-modes` and in `dn-reinstall-essentials`.

8. **Partly.** Now set with `setopt` after the definition, but still from inside the `lsp-mode` `:config`; item 15 in Part 2 (move Nix bits to `lang/nix.el`) covers the rest. **`lsp-nix-nil-flake-impure` is set in `:custom` before it is defined in
   `:config`** (`init-programming.el:348,350`). It works (live value is `t`)
   because `defcustom` keeps an already-bound value, but it reads backwards.
   Move the `lsp-defcustom` into a `with-eval-after-load 'lsp-mode`, or set
   the value after the definition.

9. **Done.** **`init-auctex.el`: `:after (tex)` on `tex-site`.** `:mode` still installs
   the `auto-mode-alist` entry up front, and the `:after` body runs whenever
   `tex` first loads, so this works. It is just an odd way to say "configure
   AUCTeX after it loads"; `use-package tex :straight auctex` would be
   clearer. `reftex-plug-into-AUCTeX` is set twice in that block.

10. **Done.** **`config-require` leaks a global variable.** `functions.el:86-87` does
    `(setq filename …)` on a name that is not in the `let`; with
    lexical-binding this creates a dynamic global `filename`.

11. **Done.** **`.emacs` duplicates `config-root`/`config-dir` definitions.** The
    `let` binding of `config-root` (line 31) is shadowed immediately by a
    `defconst` of the same name; the three `defconst`s are then redefined
    as `defcustom`s by `config/variables.el`. Keep one definition.

12. **Done** (README now documents the precedence). **README and code disagree on `init-post.el`/custom-file order.** README
    says `init-post.el` runs "just before loading the custom file"; `.emacs`
    loads `custom.el` at line 56, before every module. This also means every
    `:custom` in a `use-package` block overrides whatever the user saved via
    Customize, since `customize-set-variable` runs after `custom.el`. Decide
    which should win and make the README match.

13. **Done** (copilot-chat removed). **`copilot-chat` keybindings are never installed.** The block has
    `:after (request org markdown-mode)`, which also wraps `:bind`, and
    nothing loads `request` until copilot-chat itself does. In the live
    session `request` is not loaded and `where-is-internal
    'copilot-chat-fix` is nil, so `C-c c f`/`o`/`y`/… (`init-llm.el:94-98`)
    do nothing. Drop `:after` (`:bind` already defers the load). Once
    bound, the chords would also clash with `programming/cpp.el:88`, which
    binds `C-c c` to `recompile` in `c-mode-base-map`; a dedicated prefix
    map avoids that.

14. **Done.** **`snippets/cmake-mode/.yas-parents` contains `"cmake-mode"`** (quoted,
    and naming itself). A snippet directory cannot be its own parent and
    yasnippet reads the token verbatim, so it looks for a mode literally
    named `"cmake-mode"`. Delete the file. `cmake-ts-mode/.yas-parents`
    correctly points at `cmake-mode`.

15. **Done.** **Network I/O as a side effect of loading.** `init-docs.el:44` calls
    `(devdocs-update-all)` in `:config`, and `devdocs` is loaded eagerly, so
    this runs on every launch. `python.el:166` shells out to `npm outdated
    -g` from `lsp-pyright`'s `:config`, i.e. on the first Python buffer of
    each session. Both belong in an interactive command. The
    `lsp-cleanup-workspaces-nonexistent` call in
    `init-programming.el:437` does *not* run at startup (`:commands` defers
    the package); it runs on the first call of any of its commands, which
    makes the auto-cleanup mostly inert.

### B. Dead, duplicated and obsolete code

16. **Done.** **Package manager leftovers.** `init-custom-functions.el` still carries
    `dn-recompile-elpa`, `dn-reinstall-essentials`, and
    `dn-reinstall-all-activated-packages`, all built on `package.el`
    (`package-user-dir`, `package-reinstall`, `package-activated-list`). The
    config moved to straight; these can't work. The "essentials" list also
    names `counsel`, `ivy`, `lsp-ivy`, which are gone. `init-package.el`
    keeps the commented-out MELPA/`package-install` bootstrap.

17. **Done.** **Ivy remnants.** `all-the-icons-ivy` (`init-misc.el:79`) is installed
    although nothing else uses ivy, and it pulls ivy itself back in (`ivy`
    is loaded in the live session). `all-the-icons` is installed while
    dashboard uses `nerd-icons`.

18. **Done.** **The same package configured twice:**
    - `nix-mode`: `init-programming.el:235` and `programming/nix.el:72`
    - `gitlab-ci-mode`: `init-programming.el:207` and `programming/gitlab.el`
    - `make-mode`: `programming/build-systems.el:101` and
      `programming/makefile.el`
    - `exec-path-from-shell`: two blocks in `init-env.el` (see item 1).
    - difftastic: the `difftastic` package in `init-magit.el:240` (`D`/`S`)
      **and** a hand-rolled `dn/magit-*-with-difftastic` in
      `programming/diff.el` (`#` → `d`/`s`). They don't clash, but
      `diff.el` is not a language, hard-`require`s magit at startup, and
      hardcodes `--background=light`. Keep one.

19. **Done** (file and block deleted). **`explain-pause-mode.el` (3,699 lines) is loaded but never enabled.**
    `init-emacs.el:86` declares it with no `:config`, `:commands` or
    `:defer`. Either drop the file or install it from its GitHub recipe on
    demand (`:commands explain-pause-mode`).

20. **Done** (deleted). **`init-org.el` is disabled in `.emacs` but still tracked.** It also has
    `org-log-done` set twice, quoted lambdas (`'(lambda …)`), a nested
    `custom-set-variables` inside `:config`, and relies on the default
    `org-directory` (`~/org`) without saying so. Either fix and re-enable, or delete it (git keeps it).

21. **Done.** **Commented-out blocks** (12 `use-package` forms, plus the irony/octave/
    eglot experiments, ~150 lines). History already preserves them; delete.

22. **Done.** **Pointless `autoload` calls inside `:config`** (`build-systems.el:114`,
    `matlab.el:45-46`, `pov-ray.el:41`). By the time `:config` runs the
    package is loaded, so the autoload is a no-op.

23. **Open** (the `org` duplicate is gone with `init-org.el`). **`(use-package use-package :straight t)`** in `init-package.el`.
    `use-package` is built into Emacs 29+; straight will clone the
    upstream repo and shadow the built-in. Also `straight-use-package 'org`
    appears in both `init-package.el` and `init-org.el`.

24. **Done** (`emojify` and `all-the-icons` removed). **`emojify`** is fully configured (`init-misc.el:53-74`) but its
    global mode is commented out, so it's ~30 lines of inert config.

25. **Done** (hook-scoped `split-height-threshold`, `pos-eol`, `completing-read` with an affixation function). **`init-magit.el`:** `(make-local-variable 'split-height-threshold)` at
    load time makes the variable buffer-local in whatever buffer happens to
    be current, which is not what was intended. `point-at-eol`/
    `point-at-bol` are obsolete (`pos-eol`/`pos-bol` or
    `line-end-position`). `conv-commit-type-prompt` calls the private
    `consult--read`.

26. **Done.** `dn-compile-in-iterm` now lives in `init-os-darwin.el` and binds itself with `with-eval-after-load`, so nothing references it on other systems. **`programming/cpp.el`:** `c-basic-indent` is set both in `:custom` and
    in a later `custom-set-variables`, and that same `custom-set-variables`
    sets `indent-tabs-mode nil` globally from inside a C++ module.
    `flycheck-clang-tidy` declares `:functions flycheck-clang-analyzer-setup`
    (copy-paste from the block above).

27. **Done.** **`programming/web.el` installs `company-web`** and pushes to
    `company-backends`, but company is not in the config (completion is
    `:capf`). `(use-package json :straight t)` in the same file is a
    built-in library, and `json.el` separately declares `json-ts-mode
    :straight t`, another built-in.

28. **Done** (buffer-local hook on `pkgbuild-mode`). **`programming/build-systems.el`** adds a global `before-save-hook` for
    PKGBUILD (guarded by `major-mode`); a mode-local hook is cleaner.

### C. Consistency and hygiene

29. **Done** (every language module now has a matching header, `init-prog-<file>` feature and footer, and the smoke test enforces it). **File headers, `provide` and "ends here" footers are out of sync** in
    most language modules. Concretely:

    | File | Header says | Provides | Footer says |
    |---|---|---|---|
    | `programming/json.el` | "C++ support" | `init-prog-json` | `cpp.el ends here` |
    | `programming/terraform.el` | "C++ support" | `init-prog-terraform` | `cpp.el ends here` |
    | `programming/windows.el` | "C++ support" | `init-prog-windows` | `windows.el ends here` |
    | `programming/yaml.el` | "C++ support" | `init-prog-yaml` | ok |
    | `programming/makefile.el` | "Initialisation for Python" | `init-prog-makefile` | `init-prog-build-systems.el` |
    | `programming/web.el` | "MATLAB/Octave" | `init-prog-web` | ok |
    | `programming/gnuplot.el` | "MATLAB/Octave" | ok | ok |
    | `programming/robotframework.el` | "ROS2 support", file name `robotframework.el.el` | `robotframework.el` | `robotframework.el.el` |
    | `programming/ros.el` | `init-ros2.el` | `init-ros2` | `init-ros2.el` |
    | `programming/cpp.el` | ok | `cpp` (no prefix) | ok |
    | `programming/nix.el` | no header at all | nothing | none |
    | `init-docs.el` | "Initialisation for programming" | ok | ok |
    | `config/variables.el` | header starts with `;;` not `;;;` | ok | ok |

    `go.el` and `markdown.el` also say `go.el`/`markdown.el` in the footer but
    provide `init-prog-*`. Four different feature-name conventions coexist (`init-prog-x`,
    `init-x`, `x`, `x.el`). This is exactly what a batch byte-compile in CI
    would catch (see Part 2, phase 0).

30. **Done.** **`Local Variables` footers with `eval:` forms** are copy-pasted into
    13 files. They `setq` the global `config-dotemacs-lisp` and `config-dir`
    whenever the file is *visited*, trigger "unsafe local variable"
    prompts, and are wrong in `programming/*` (they compute `config/`
    relative to `programming/`). Replace with a single `.dir-locals.el` at
    the repo root, or with a proper load-path so flymake/flycheck can find
    `config-functions`.

31. **Partly.** Commands that lived in other packages' namespaces (`magit-*`, `smerge-*`, `lsp-*`, `treesit-*`) are prefixed, and `defgroup dn` exists. About 25 unprefixed own functions remain: `conv-commit-*`, `add-conventional-commit-faces`, `en-abb`/`fr-abb`, `en-dic`/`fr-dic`/`de-dic`, `my-ispell-word`, `my-flyspell-auto-correct-word`, `text-mode-hook-setup`, `crm-indicator`, `imdoc*`, `formatted-copy-*`, `revert-all-buffers`, `kill-from-line-beginning`, `shutdown-emacs-server`, `reb-query-replace`, `bury-compile-buffer-if-successful`, `latex-help-get-cmd-alist`. **Namespace.** Personal symbols use `dn-`, `dn/`, `dn--`, `my-`, `pd--`,
    `conv-commit-`, `yas-lib-`, or no prefix at all (`en-abb`, `fr-abb`,
    `imdoc`, `crm-indicator`, `revert-all-buffers`, `kill-from-line-beginning`,
    `shutdown-emacs-server`, `magit-push-to-all-remotes`,
    `magit-run-mergiraf-*`, `add-conventional-commit-faces`,
    `bury-compile-buffer-if-successful`, `treesit-install-all-grammars`,
    `text-mode-hook-setup`, `lsp-update-server`, and the five `lsp-cleanup-*`/`lsp-list-workspaces` commands in `cleanup-lsp-workspaces.el`). Unprefixed `magit-*`,
    `lsp-*`, `treesit-*` and `smerge-*` names risk clashing with upstream.
    Pick one prefix (`dn-`) and one customization group (`dn`, which is
    referenced by six `defcustom`s but never `defgroup`ed).

32. **Partly.** Everything is fixed except `use-package` itself (item 23) and `savehist` in `init-completion.el`, which has no `:straight` keyword at all. **Built-in libraries declared inconsistently.** `cc-mode`, `python`,
    `make-mode`, `rst`, `lsp-nix`, `vertico-*` correctly use `:straight nil`;
    `epg`, `diff-mode`, `display-line-numbers`, `printing`, `json`,
    `json-ts-mode`, `savehist`, `use-package` do not.

33. **Partly** (`.dir-locals.el` sets `indent-tabs-mode nil`; 7 own files still contain tabs: `config/functions.el`, `init-completion.el`, `init-ui.el`, `init-magit.el`, `init-editing.el`, `init-ispell.el`, `programming/python.el`). **Formatting.** 13 files mix tabs and spaces; closing parens are
    routinely on their own line; `(if x (progn …))` instead of `when`;
    `'(lambda …)` instead of `#'`/`lambda`; `(progn …)` as the sole body of
    `:config`. A one-time pass with `indent-region` under
    `indent-tabs-mode nil` (enforced through `.dir-locals.el`) plus
    `checkdoc` would settle this.

34. **Open.** **Host-specific artifacts in the repo.** `emacs.service` hardcodes
    `/snap/bin/emacs` and `LD_LIBRARY_PATH=/usr/local/lib` with commented
    alternatives; the four `docsets/*.tgz` are Git LFS blobs (not
    fetchable in this environment) that `dn-dash-docs-install` re-installs
    from. Given the Nix/home-manager docsets, the service unit and docset
    generation probably belong in the home-manager config, with this repo
    referencing them by path.

35. **Done.** **`.emacs` trailing statement.** `(put 'narrow-to-region 'disabled nil)`
    sits after the `;;; .emacs ends here` footer.

36. **Done** (the README now covers installation, `--init-directory`, load order, the `config/` shadowing, per-host overrides, layout and the Emacs version). **README gaps.** It does not mention that `config-require` looks in
    `.emacs_lisp/config/` *first* (`functions.el:82-87`), so a gitignored
    `config/init-magit.el` silently replaces the tracked module. That is a
    useful override hook but it is undocumented and easy to trip over. The
    README also does not describe `yas-lib/`, `packages/`, `abbrev_*`, or
    which Emacs version is required (the config assumes 29+: `treesit`,
    `setopt`, built-in `use-package`).

37. **Done** for own code (`dashboard-worktrees-patch.el` became `lib/dn-dashboard-worktrees.el` with a cookie). Only the vendored `ris.el` lacks one. **Missing `lexical-binding` cookies.** Emacs 31 warns at startup for
    `dashboard-worktrees-patch.el` (tracked) and for the untracked
    `custom.el`, `config/init-post.el` and `~/.emacs.d/early-init.el`.

---

## Part 2: Modularity, structure and architecture

This part was originally a proposal. Main has since implemented it, so it
is now an assessment of what landed, what the new layout still lacks, and
what to do next.

### What exists now

```
early-init.el, init.el         entry points at the repo root (linked from ~/.emacs.d or --init-directory)
.emacs                         loads config/, init-pre, custom.el, dn-modules, init-post
.emacs_lisp/
  config/variables.el          defgroup dn; paths; dn-modules; dn-disabled-languages
  config/functions.el          config-require, config-load-file-exec-func, dn-load-directory,
                               config-when-system, dn-async-process
  config/init-pre.el, init-post.el, custom.el   (gitignored) per-host overrides
  init-package.el              straight bootstrap, use-package defaults
  init-env.el                  exec-path-from-shell, keychain, 1Password, aio
  init-ui.el                   theme, which-key, hl-line+, dashboard (+ lib/dn-dashboard-worktrees), helpful, line numbers
  init-completion.el           vertico, consult, orderless, marginalia, embark, prescient, hotfuzz, wgrep
  init-editing.el              browse-kill-ring, smart-shift, whitespace-cleanup, abbrev, yasnippet, yas-lib
  init-project.el              projectile, bufler, ztree, dtrt-indent, direnv, editorconfig, project-directory
  init-docs.el, init-multiple-cursors.el, init-magit.el, init-mergiraf.el, init-llm.el
  init-lsp.el                  lsp-mode, lsp-ui, dap, lsp-booster, treemacs, workspace cleanup
  init-programming.el          flycheck, compilation helpers, treesit sources, format-all, datetime, logview,
                               then dn-load-directory over programming/
  init-auctex.el, init-ispell.el, init-misc.el
  init-os-darwin.el            every darwin-only bit, loaded only on darwin via dn-modules
  init-keybindings.el, init-custom-functions.el, init-custom.el
  programming/<lang>.el        22 language modules, each providing init-prog-<lang>
  lib/                         own libraries (dn-dashboard-worktrees, cleanup-lsp-workspaces)
  vendor/                      hl-line+, ris, project-directory, cmake-format
  packages/, yas-lib/, snippets/, abbrev_*, docsets/
Makefile, test/smoke.el        make check: batch load through early-init.el + init.el, fail on warnings
```

### What the restructure achieved

Measured against the mechanisms proposed in the first version of this
review:

- **One loader, one naming rule.** `dn-load-directory` requires each file
  by path as `init-prog-<base>`, so a wrong `provide` is a load error, and
  the smoke test checks every module independently. The `intern-soft`
  heuristic is gone.
- **A module list instead of a require chain.** `dn-modules` and
  `dn-disabled-languages` are `defcustom`s that `init-pre.el` can override
  per host; `skip.txt` is gone. `init-os-darwin` is appended to the list
  only on darwin.
- **OS-specific code in one place.** The only darwin checks outside
  `init-os-darwin.el` are the computed variable list in `init-env.el`, the
  `dn-modules` default, and the pyright hook in `programming/python.el`.
- **Own, patched and vendored code separated.** `vendor/` holds pristine
  third-party files, `lib/` holds own libraries, and the dashboard patch
  became a proper library with a test for its porcelain parser.
- **Module boundaries match names.** `init-emacs.el` is gone; UI,
  completion, editing and project each have a file, and LSP tooling has its
  own module instead of sharing one with the language loader.
- **Self-contained entry points.** `init.el` and `early-init.el` at the
  root work both symlinked and via `--init-directory`; `early-init.el`
  also owns `LSP_USE_PLISTS`, which used to live in `emacs.service`.
- **Verification.** `make check` goes through the real entry points,
  fails on `use-package` warnings, and asserts that `lsp-mode`, `yasnippet`
  and `devdocs` stay deferred.

### Remaining structural gaps

1. **`init-custom.el` is still loaded outside `dn-modules`** (hardcoded in
   `.emacs`), still misnamed, and holds two unrelated things:
   `dn-lsp-mode-disabled`, which only `init-lsp.el` reads, and
   `dn-editorconfig-major-mode-hook`, which nothing calls. Move the
   variable into `init-lsp.el` (or `config/variables.el` beside the other
   `defcustom`s), delete the unused hook, and drop the module.

2. **Three leftover grab-bag modules.** `init-misc.el` (epg, pacfiles,
   printing), `init-keybindings.el` (three global bindings) and
   `init-custom-functions.el` (one command, one clang-format hook) are each
   under 70 lines and have no theme. Fold them into `init-editing.el`,
   `init-ui.el` and `programming/cpp.el` respectively. The same applies to
   the single-package `init-multiple-cursors.el` (editing) and
   `init-mergiraf.el` (magit/vcs).

3. **Two infrastructure prefixes.** `config-require`,
   `config-load-file-exec-func` and `config-when-system` coexist with
   `dn-load-directory`, `dn-modules` and `dn-async-process`, and the
   `config` and `dn` customization groups both exist. Pick `dn-` and
   rename the four `config-*` helpers; `config-when-system` can become
   `(when (memq system-type (ensure-list TYPE)) …)`.

4. **`.emacs` is now an implementation detail.** `init.el` only loads it.
   Its 30 lines could live in `init.el` directly, which would also remove
   the dot-file from the repo root and the `file-truename` indirection.

5. **`early-init.el` pins `user-emacs-directory` to `~/.emacs.d`.** This
   is deliberate, so several checkouts share straight's builds, but it
   means `--init-directory` and `make check` are not hermetic: a fresh
   machine still needs a bootstrapped `~/.emacs.d`. Worth documenting in
   the README's checking section, and worth an `EMACS_USER_DIR` override
   for CI.

6. **Deferral is still opt-in.** `use-package-always-defer` is nil and
   `straight-use-package-by-default` is unset; 56 of 123 blocks have no
   lazy trigger. Three packages are verified deferred by the test. A
   `use-package-compute-statistics` run followed by `:defer t` on the
   long tail is the remaining startup lever, and each newly deferred
   package can be added to the smoke test's deferral list.

7. **No byte-compile, no CI.** `make check` catches load errors and
   warnings but not the class of problems `byte-compile-file` finds
   (unused variables, wrong arities, obsolete calls). A `make compile`
   target over `config/`, `init-*.el`, `programming/`, `lib/` with
   `byte-compile-error-on-warn`, and a GitHub Actions job running both,
   closes phase 0.

8. **Implicit dependencies remain in a few places.** `programming/python.el`
   and `init-custom-functions.el` call `projectile-project-root` without
   requiring `init-project`; `init-docs.el` binds consult commands; the
   language modules assume `init-lsp` and `init-docs` loaded first. This is
   fine while `dn-modules` fixes the order, but a `(require 'init-lsp)` at
   the top of a module that needs it makes the order explicit and lets a
   host drop modules from `dn-modules` safely.

### Migration plan status

| Phase | Status |
|---|---|
| 0. Safety net | Mostly done: `make check`, `.dir-locals.el`, naming rule enforced. Missing: byte-compile target, CI. |
| 1. Delete | Done. |
| 2. Fix bugs | Done. |
| 3. Re-cut modules | Done: `init-emacs.el` split, LSP extracted, languages moved, `os-darwin`, `vendor/` + `lib/`. |
| 4. Loader + naming | Done for the loader, module list and cross-package command names. Left: the `config-` helpers and the unprefixed own functions (item 31). |
| 5. Init directory | Done: `early-init.el`, `init.el`, `--init-directory`, README. Left: the defer audit and `straight-use-package-by-default`. |

### Next steps, in order

1. Add `make compile` (byte-compile own modules, warnings as errors) and a
   CI workflow that runs `make check` and `make compile` on Emacs 29 and
   the current release.
2. Fold `init-custom.el`, `init-misc.el`, `init-keybindings.el`,
   `init-custom-functions.el`, `init-multiple-cursors.el` and
   `init-mergiraf.el` into their natural homes; update `dn-modules` and
   the README layout table.
3. Rename the `config-*` helpers to `dn-*`, merge the `config` group into
   `dn`, and prefix the remaining own functions (item 31).
4. Run `use-package-compute-statistics`, defer the long tail, extend the
   smoke test's deferral list as each package moves.
5. Re-indent the seven files that still contain tabs; add `:straight nil`
   to `savehist` and `use-package`; unnest the ispell setup; move
   `emacs.service` out of the repo or template its paths.
