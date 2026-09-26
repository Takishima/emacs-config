# Emacs config review: cleanup and modularity

Scope: every tracked `.el` file, the entry point, README, snippets layout,
service file and repo metadata, originally as of `47ec94c`, re-assessed against main `a6f940d` after the
40 cleanup commits that followed the first pass. Findings were
checked against a running Emacs 31.1 session with this config loaded (via
`emacsclient`). Items marked **verify** still need a manual repro. Each
numbered item now starts with its status: **Done** (fixed on main),
**Partly** (some of it landed) or **Open**.

Numbers that frame the rest of the review:

| Metric | Value |
|---|---|
| `use-package` declarations | 144 (was 155) |
| Own init modules (`init-*.el`) | 16 (`init-org.el` deleted) |
| Language modules (`programming/*.el`) | 20 (two skipped; `diff.el` and `makefile.el` removed) |
| Largest own module | `init-emacs.el`, 770 lines |
| Vendored third-party code | 4 files, ~950 lines (`explain-pause-mode.el` deleted) |
| Own top-level helper libraries | 2 (`dashboard-worktrees-patch.el`, `cleanup-lsp-workspaces.el`) |
| Commented-out `use-package` blocks | 0 (was 12) |

## Status after main `a6f940d`

Main now carries most of migration phases 1 and 2 from Part 2, plus a
batch smoke test (`test/smoke.el`). Of the 37 cleanup items:

| Status | Items |
|---|---|
| Done | 1, 3-7, 9-22, 24, 25, 27, 28, 35 |
| Partly | 2, 8, 26, 31, 32 |
| Open | 23, 29, 30, 33, 34, 36, 37 |

The smoke test covers exactly the regressions that were fixed (feature
name, `filename` leak, ispell args, ts-mode bindings and docsets, AUCTeX
commands), which is the right shape for phase 0. It still needs a
`Makefile` target and CI wiring, and it is not hermetic: it loads `.emacs`,
so it needs a bootstrapped `straight/` directory with every package already
cloned. A byte-compile pass over the own modules is the missing half.

Section A and section B are now closed apart from the built-in
`use-package` declaration (23). What remains is section C, in order of
value: the header/provide/footer drift (29), the `Local Variables` footers
(30), the namespace cleanup (31), the `lexical-binding` cookie (37), and
the formatting pass (33). Everything in Part 2 still applies, and with the
deletions done, phase 3 (re-cutting the modules) is the next real step.

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

23. **Open.** **`(use-package use-package :straight t)`** in `init-package.el`.
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

26. **Partly.** The duplicate `c-basic-indent`, the `:functions` line, and the global `indent-tabs-mode` (now a `setq-default` in `init-emacs.el`) are fixed. Still open: `compile-in-iterm` is bound in four keymaps in `cpp.el` and in `rest.el` but only defined on darwin. **`programming/cpp.el`:** `c-basic-indent` is set both in `:custom` and
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

29. **Open** (only `python.el` was fixed). **File headers, `provide` and "ends here" footers are out of sync** in
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

30. **Open.** **`Local Variables` footers with `eval:` forms** are copy-pasted into
    13 files. They `setq` the global `config-dotemacs-lisp` and `config-dir`
    whenever the file is *visited*, trigger "unsafe local variable"
    prompts, and are wrong in `programming/*` (they compute `config/`
    relative to `programming/`). Replace with a single `.dir-locals.el` at
    the repo root, or with a proper load-path so flymake/flycheck can find
    `config-functions`.

31. **Partly** (`defgroup dn` exists now; names unchanged). **Namespace.** Personal symbols use `dn-`, `dn/`, `dn--`, `my-`, `pd--`,
    `conv-commit-`, `yas-lib-`, or no prefix at all (`en-abb`, `fr-abb`,
    `imdoc`, `crm-indicator`, `revert-all-buffers`, `kill-from-line-beginning`,
    `shutdown-emacs-server`, `magit-push-to-all-remotes`,
    `magit-run-mergiraf-*`, `add-conventional-commit-faces`,
    `bury-compile-buffer-if-successful`, `treesit-install-all-grammars`,
    `text-mode-hook-setup`, `lsp-update-server`, and the five `lsp-cleanup-*`/`lsp-list-workspaces` commands in `cleanup-lsp-workspaces.el`). Unprefixed `magit-*`,
    `lsp-*`, `treesit-*` and `smerge-*` names risk clashing with upstream.
    Pick one prefix (`dn-`) and one customization group (`dn`, which is
    referenced by six `defcustom`s but never `defgroup`ed).

32. **Partly.** `epg`, `diff-mode`, `display-line-numbers`, `printing`, `json`, `json-ts-mode` are fixed; `use-package` itself is still `:straight t` and `savehist` has no `:straight` at all. **Built-in libraries declared inconsistently.** `cc-mode`, `python`,
    `make-mode`, `rst`, `lsp-nix`, `vertico-*` correctly use `:straight nil`;
    `epg`, `diff-mode`, `display-line-numbers`, `printing`, `json`,
    `json-ts-mode`, `savehist`, `use-package` do not.

33. **Open.** **Formatting.** 13 files mix tabs and spaces; closing parens are
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

36. **Open** (the custom-file paragraph was added; the rest is not). **README gaps.** It does not mention that `config-require` looks in
    `.emacs_lisp/config/` *first* (`functions.el:82-87`), so a gitignored
    `config/init-magit.el` silently replaces the tracked module. That is a
    useful override hook but it is undocumented and easy to trip over. The
    README also does not describe `yas-lib/`, `packages/`, `abbrev_*`, or
    which Emacs version is required (the config assumes 29+: `treesit`,
    `setopt`, built-in `use-package`).

37. **Open.** **Missing `lexical-binding` cookies.** Emacs 31 warns at startup for
    `dashboard-worktrees-patch.el` (tracked) and for the untracked
    `custom.el`, `config/init-post.el` and `~/.emacs.d/early-init.el`.

---

## Part 2: Modularity, structure and architecture

### What exists today

```
.emacs                          entry point (paths, ordered config-require list)
.emacs_lisp/
  config/variables.el           paths as defcustoms
  config/functions.el           config-require, config-load-file-exec-func, config-when-system
  config/init-pre.el, init-post.el   (gitignored) user hooks
  custom.el                     (gitignored)
  init-*.el                     17 topic modules, loaded in a fixed order
  programming/*.el              auto-discovered language modules, skip.txt opt-out
  yas-lib/*.el                  auto-discovered snippet helpers
  packages/<name>/              two own packages (ros2)
  *.el at top level             5 vendored third-party files + 1 patch
  snippets/, abbrev_*, docsets/
```

**What works well and should be kept:**
- `use-package` + straight everywhere, so each block is self-describing.
- The idea of per-topic and per-language files, with automatic discovery.
- Explicit, gitignored extension points (`init-pre`, `init-post`,
  `custom.el`, `skip.txt`, `config/` shadowing).
- Helper macros for OS branching.

**Where modularity breaks down:**

1. **Module boundaries do not match the file names.** `init-emacs.el`
   holds the theme, UI toggles, the entire minibuffer completion stack
   (vertico/consult/orderless/marginalia/embark, ~350 lines), dashboard,
   bufler, 1Password, `aio`, font-size helpers and generic utility
   functions. `init-completion.el` holds abbrevs, prescient and yasnippet,
   but not completion. `init-programming.el` mixes cross-language tooling
   (lsp/dap/treesit/flycheck/editorconfig/compilation) with the language
   loader and with grab-bag packages that are themselves languages
   (`ansible`, `hcl-mode`, `nix-mode`, `gitlab-ci-mode`, `format-all`).
   `init-custom.el` is not about `custom.el`. `init-llm.el` also owns the
   terminals (`vterm`, `eat`).

2. **Two discovery mechanisms, four naming conventions, one heuristic.**
   Top-level modules are listed by hand in `.emacs`; language and yas-lib
   modules are globbed. The glob loader decides between `require` and
   `load` with `intern-soft`, i.e. based on whether a symbol happens to be
   interned yet. That is why the wrong `provide` in `python.el` goes
   unnoticed.

3. **Implicit cross-module coupling through load order.**
   `dn-lsp-mode-disabled` (defined in `init-custom`) is used in
   `init-programming`; `dn-async-process` (in `init-emacs`) in
   `programming/python.el`; `compile-in-iterm` (in `init-programming`,
   darwin only) is bound in `cpp.el` and `rest.el` on every OS;
   `projectile-project-root` is used in `python.el` and
   `init-custom-functions.el`; `consult` is assumed present by
   `init-docs`/`init-magit`. None of these declare the dependency; they
   work because `.emacs` happens to order the requires that way.

4. **OS-specific code is smeared across 9 files** (`config-when-system` /
   `system-type` checks in env, keybindings, misc, auctex, emacs,
   programming, cpp, build-systems, python). There is no single place to
   look for "what changes on macOS".

5. **Own code, patches and vendored code share one directory.**
   `hl-line+.el`, `ris.el`, `project-directory.el`, `cmake-format.el` (all
   third-party) sit next to `init-*.el`,
   `dashboard-worktrees-patch.el` and the new own library
   `cleanup-lsp-workspaces.el`. Nothing marks which files are edited
   locally, which are pristine copies, and which could be straight recipes.

6. **Feature toggles are all-or-nothing edits to tracked files.** The only
   knobs are `skip.txt` (tracked, so per-host choices become commits) and
   commenting out a `config-require` in `.emacs`. There is no way for
   `init-pre.el` to say "no LLM module on this machine".

7. **Startup is fully eager.** `use-package-always-defer` is nil, most
   blocks have no `:defer`/`:commands`/`:hook`, `yas-reload-all` and
   `devdocs-update-all` run at boot, and `gc-cons-threshold` is tuned as a
   side effect of the `lsp-mode` block. The repo has no `early-init.el`;
   the host has one in `~/.emacs.d/` that lives outside this repo.

8. **Verification is only half there.** `test/smoke.el` now batch-loads the
   config and checks the fixed regressions, but nothing runs it (no
   `Makefile`, no CI) and nothing byte-compiles the own modules. Items 29
   and 37 in Part 1 would be caught by `byte-compile-file` with
   `byte-compile-error-on-warn`, which the smoke test cannot see.

### Proposed target structure

The proposal keeps every idea that works and changes the layout so that
each concern has exactly one home, dependencies are declared, and the repo
can be tested in isolation.

```
emacs-config/
├── early-init.el            gc during startup, package-enable-at-startup nil, UI flags
├── init.el                  ~40 lines: paths, straight bootstrap, load core, run module list
├── lisp/
│   ├── core/
│   │   ├── dn-paths.el      defgroup dn; dn-root, dn-lisp-dir, dn-local-dir …
│   │   ├── dn-lib.el        dn-require, dn-load-directory, dn-when-system, dn-async-process,
│   │   │                    generic commands (revert-all-buffers, kill-from-line-beginning…)
│   │   └── dn-packages.el   straight bootstrap, use-package defaults, diminish, system-packages
│   ├── modules/             one concern per file, each (provide 'dn-mod-<name>)
│   │   ├── ui.el            theme, which-key, dashboard (+ worktrees patch), line numbers, hl-line, fonts
│   │   ├── completion.el    vertico, consult, orderless, marginalia, embark, prescient, hotfuzz
│   │   ├── editing.el       multiple-cursors, smart-shift, whitespace-cleanup, subword, abbrev, yasnippet, yas-lib
│   │   ├── project.el       projectile, bufler, direnv, editorconfig, dtrt-indent
│   │   ├── vcs.el           magit + extensions, conventional commits, mergiraf (magit + smerge), difftastic
│   │   ├── lsp.el           lsp-mode, lsp-ui, dap, lsp-booster, treesit, flycheck, compilation helpers
│   │   ├── docs.el          devdocs, dash-docs, helpful
│   │   ├── spell.el         ispell/flyspell/flycheck-aspell, lsp-ltex
│   │   ├── tex.el           auctex, ris
│   │   ├── ai.el            copilot, copilot-chat, aidermacs, claude-code
│   │   ├── shell.el         vterm, eat, exec-path-from-shell, keychain
│   │   ├── org.el           (if kept)
│   │   ├── os-darwin.el     every darwin-only bit: modifiers, Swiss keyboard, Skim, texbin, plist, iTerm
│   │   └── os-linux.el
│   ├── lang/                renamed programming/, each (provide 'dn-lang-<name>)
│   ├── lib/                 own reusable libraries: cleanup-lsp-workspaces.el (as dn-lsp-workspaces.el)
│   ├── vendor/              pristine third-party: hl-line+, ris, project-directory, cmake-format
│   ├── patches/             dashboard-worktrees-patch.el (files that modify a package after load)
│   └── packages/            own packages (ros2-*), unchanged
├── etc/                     snippets/, abbrev/, docsets/  (data, not code)
├── local/                   gitignored: init-pre.el, init-post.el, custom.el, module overrides
├── Makefile                 make check  → batch load + byte-compile + checkdoc
└── .github/workflows/ci.yml
```

Key mechanisms behind the layout:

**a. One loader, one naming rule.** `dn-load-directory DIR PREFIX &optional
SKIP` sorts the directory, and for `foo.el` does
`(require (intern (concat PREFIX "foo")) file)`. The convention "file
`lang/cpp.el` provides `dn-lang-cpp`" is enforced, not guessed. Replace
`intern-soft` with this and the `python.el` mistake becomes an immediate
load error.

**b. A module list instead of a hand-written require chain.**

```elisp
(defcustom dn-modules
  '(ui completion editing project vcs lsp docs spell tex ai shell)
  "Modules to load, in order. Override in local/init-pre.el.")
(defcustom dn-disabled-languages '(matlab gnuplot) "…")
```

`init.el` loads `local/init-pre.el` first, then iterates `dn-modules`. A
host that wants no AI tooling sets `(setq dn-modules (remq 'ai dn-modules))`
in its untracked `init-pre.el`. `skip.txt` becomes `dn-disabled-languages`
and stops being a tracked file that encodes per-host choices.

**c. Explicit dependencies.** Every module starts with `(require 'dn-lib)`
and, where it uses another module's symbols, `(require 'dn-mod-lsp)` (or a
`declare-function`/`defvar` for soft dependencies). Shared helpers
(`dn-async-process`, `compile-in-iterm`, `dn-lsp-mode-disabled`) move to
`dn-lib.el` or to the module that owns the concept, never to a "misc"
file. Packages that other modules assume (`consult`, `projectile`,
`s`, `f`) are declared with `:straight t` where they are first used.

**d. OS modules instead of scattered branches.** `os-darwin.el` is only
loaded when `system-type` is `darwin` and owns everything currently behind
`config-when-system 'darwin`. Language modules stop binding
`compile-in-iterm` on Linux; `os-darwin.el` adds the binding to
`c++-mode-map` with `with-eval-after-load`.

**e. A self-contained init directory.** With `early-init.el` and
`init.el` at the repo root, the config runs with
`emacs --init-directory ~/emacs-config` (Emacs 29+) with no symlinks, and
CI can run it in a clean container. straight's `straight/` and
`eln-cache/` land inside the repo and are gitignored. The
`config-dotemacs-d` variable and the `.emacs` vs `.emacs.d` split go away.

**f. Straight defaults.** Set `straight-use-package-by-default t` and use
`:straight nil` only for built-ins. Consider `:defer t` as the default with
`:demand t` on the handful of always-on modes (vertico, which-key,
projectile, editorconfig); with 149 packages this is the single biggest
startup-time lever. `use-package-compute-statistics` + `M-x
use-package-report` shows where the time goes before touching anything.

**g. Verification.** `make check` runs
`emacs --batch --init-directory . --eval '(kill-emacs 0)'` (fails on any
load error) and byte-compiles `lisp/core lisp/modules lisp/lang` with
`byte-compile-error-on-warn`. A GitHub Actions job with `purcell/setup-emacs`
on Emacs 29 and 30 makes provide/header drift impossible to merge.

### Migration plan

Each phase is one PR, independently mergeable, with the config working at
every step.

| Phase | Content | Risk |
|---|---|---|
| 0. Safety net (**partly done**: smoke test exists) | Wire `test/smoke.el` into a `Makefile` + CI; add byte-compile of own modules; `.dir-locals.el`; fix the provide/header table (item 29); remove `Local Variables` footers. | none |
| 1. Delete (**done**) | Only `(use-package use-package :straight t)` (23) is left. | none |
| 2. Fix bugs (**done**) | Items 1-15 landed on main, as did 22, 25, 28. The one leftover is the cross-OS `compile-in-iterm` binding (26). | low |
| 3. Re-cut modules | Split `init-emacs.el` into `ui`/`completion`/`editing`; move tooling out of `init-programming.el` into `lsp.el`; move language packages into `lang/`; create `os-darwin.el`; `lib/` + `vendor/` + `patches/` split. Pure moves, no behaviour change. | low |
| 4. Loader + naming | `dn-` prefix everywhere; `defgroup dn`; single `dn-load-directory`; `dn-modules`/`dn-disabled-languages`; README rewrite. | medium |
| 5. Init directory | `early-init.el` + `init.el`, `--init-directory` support, custom-file ordering fix, `straight-use-package-by-default`, defer audit. | medium |

Phases 0-2 are worth doing regardless of whether the restructuring in 3-5
is adopted.

### Quick wins (under an hour total)

Everything from the original quick-wins list has landed except these:

- Add a `Makefile` with `check: emacs --batch -l .emacs -l test/smoke.el`
  and a byte-compile target, and a CI job that runs it.
- Fix the header/footer drift in `json.el`, `terraform.el`, `windows.el`,
  `yaml.el`, `web.el`, `gnuplot.el`, `robotframework.el`, `ros.el`,
  `markdown.el`, `go.el`, and give `nix.el` a header.
- Delete the 15 `Local Variables` footers.
- Replace `(use-package use-package :straight t)` with `:straight nil`, and
  add `:straight nil` to `savehist`.
- Guard the `compile-in-iterm` bindings in `cpp.el` and `rest.el` with the
  darwin check, or define a no-op elsewhere.
- Add the `lexical-binding` cookie to `dashboard-worktrees-patch.el`.
