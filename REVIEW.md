# Emacs config review: cleanup and modularity

Scope: every tracked `.el` file, the entry point, README, snippets layout,
service file and repo metadata, as of `47ec94c` (main after the Go, claude-code-ide, sops and LSP-workspace changes). No Emacs binary was
available in the review environment, so nothing below was executed. Items
marked **verify** are strong suspicions that need a live Emacs to confirm.

Numbers that frame the rest of the review:

| Metric | Value |
|---|---|
| `use-package` declarations | 155 |
| Own init modules (`init-*.el`) | 17 (one disabled) |
| Language modules (`programming/*.el`) | 22 (two skipped) |
| Largest own module | `init-emacs.el`, 779 lines |
| Vendored third-party code | 5 files, ~4,600 lines (3,699 in `explain-pause-mode.el`) |
| Own top-level helper libraries | 2 (`dashboard-worktrees-patch.el`, `cleanup-lsp-workspaces.el`) |
| Commented-out `use-package` blocks | 12 (aidermacs, irony, elpy, eglot, cmake-ide, rtags, lsp-bridge, prism, code-review, ruff-lsp, flycheck-irony, tex) |

---

## Part 1: Cleanup

Ordered by impact. Section A is things that are wrong today, B is dead or
duplicated code, C is consistency and hygiene.

### A. Bugs and latent breakage

1. **`programming/python.el` is very likely not applied at all.** (**verify**)
   The `(use-package python …)` block uses a `:dash` keyword
   (`python.el:59`). `:dash` is not a stock `use-package` keyword and nothing
   in the repo defines one. `use-package` catches parse errors and turns the
   whole form into a warning (check `*Warnings*` for "Failed to parse package
   python"). If so, the python hooks, the ipython shell settings, the docs
   hooks and the `C-c i` binding are all silently dropped. The same block
   also has two `:config` sections. The same caveat applies to
   `:ensure-system-package` in `init-magit.el` and `init-ispell.el`: it
   only works if `use-package-ensure-system-package` is loaded, and nothing
   requires it explicitly.

2. **`init-ispell.el` overrides its own spell-checker arguments.** Line 100
   computes `ispell-extra-args` from `flyspell-detect-ispell-args`, then line
   127 unconditionally does `(setq-default ispell-extra-args '("--reverse"))`,
   discarding the language and camel-case flags. Also the entire ispell
   setup, including the hunspell fallback branch, lives inside
   `flycheck-aspell`'s `:config`, so if aspell is absent the fallback it
   describes never runs.

3. **`programming/python.el` provides the wrong feature.** It ends with
   `(provide 'init-prog-build-systems)`, copied from `build-systems.el`. It
   only loads today because the discovery loop in `init-programming.el`
   uses `intern-soft`, which falls back to `load` when the symbol is not
   yet interned. Any change that interns `init-prog-python` first (for
   example mentioning it in another file) would flip the loop to `require`
   and it would fail with "Required feature was not provided".

4. **`programming/matlab.el` redefines the built-in `string-replace`**
   (line 57). Emacs 28+ ships `string-replace` with the same arity, so the
   redefinition currently works by luck. It is skipped via `skip.txt`, but
   it is a trap if the skip is ever removed. Same file uses the obsolete
   `string-as-unibyte`.

5. **`init-emacs.el` `dn-cmd-after-saved-file` is broken logic.** It
   computes `command` then calls `(shell-command (cdr match))` ignoring it,
   `add-to-list` on a `let`-bound list never updates the executed command,
   and the warning prints `command`, which is `nil` on the failure path.
   `dn-script-on-save` is also empty, so the hook runs for nothing on every
   save. Delete or rewrite.

6. **Undeclared runtime dependencies** that only work because some other
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

7. **`magit-popup` is obsolete and the one consumer is probably broken.**
   `python-pytest` switched to transient years ago, so
   `magit-define-popup-option 'python-pytest-dispatch …`
   (`python.el:244-250`) targets a transient prefix. Together with item 1
   this whole block is suspect. `magit-popup` is also listed in
   `emojify-inhibit-major-modes` and in `dn-reinstall-essentials`.

8. **`lsp-nix-nil-flake-impure` is set in `:custom` before it is defined in
   `:config`** (`init-programming.el:347,349`). It works only because
   `custom-initialize-reset` keeps an already-bound value. Move the
   `lsp-defcustom` into `:init` or a `with-eval-after-load 'lsp-mode`, or
   set the value after the definition.

9. **`init-auctex.el`: `:after (tex)` on `tex-site`** (**verify**). `tex-site`
   is the file that sets up the autoloads that eventually load `tex`, so
   waiting for `tex` before configuring `tex-site` may mean the
   `:custom`/`:config` sections only run after a `.tex` file is opened, or
   never if `tex` is loaded by another path. `reftex-plug-into-AUCTeX` is
   set twice in that block.

10. **`config-require` leaks a global variable.** `functions.el:86-87` does
    `(setq filename …)` on a name that is not in the `let`; with
    lexical-binding this creates a dynamic global `filename`.

11. **`.emacs` duplicates `config-root`/`config-dir` definitions.** The
    `let` binding of `config-root` (line 31) is shadowed immediately by a
    `defconst` of the same name; the three `defconst`s are then redefined
    as `defcustom`s by `config/variables.el`. Keep one definition.

12. **README and code disagree on `init-post.el`/custom-file order.** README
    says `init-post.el` runs "just before loading the custom file"; `.emacs`
    loads `custom.el` at line 56, before every module. This also means every
    `:custom` in a `use-package` block overrides whatever the user saved via
    Customize, since `customize-set-variable` runs after `custom.el`. Decide
    which should win and make the README match.

13. **`copilot-chat` still squats on the `C-c c` prefix.** Main has since
    moved Claude to `C-c C-'` (`init-llm.el:72`), which removed the direct
    clash with the old `claude-code-command-map`, but `C-c c f`, `C-c c o`,
    `C-c c y` … (`init-llm.el:94-98`) are still bound in `global-map`
    rather than a dedicated keymap, and `programming/cpp.el:88` binds
    `C-c c` to `recompile` in `c-mode-base-map`, so in C/C++ buffers the
    copilot-chat chords are shadowed. A single `dn-ai-map` prefix would
    settle this.

14. **`snippets/cmake-mode/.yas-parents` contains `"cmake-mode"`** (quoted,
    and naming itself). A snippet directory cannot be its own parent and
    yasnippet reads the token verbatim, so it looks for a mode literally
    named `"cmake-mode"`. Delete the file. `cmake-ts-mode/.yas-parents`
    correctly points at `cmake-mode`.

15. **Startup does network I/O.** `init-docs.el:44` calls
    `(devdocs-update-all)` in `:config` on every launch, and
    `python.el:166` shells out to `npm outdated -g` on every start via
    `dn-async-process`. Both belong in an interactive command. In the same
    spirit, `init-programming.el:426-437` now runs
    `lsp-cleanup-workspaces-nonexistent` (filesystem walk over every
    remembered workspace) from `:config` at startup.

### B. Dead, duplicated and obsolete code

16. **Package manager leftovers.** `init-custom-functions.el` still carries
    `dn-recompile-elpa`, `dn-reinstall-essentials`, and
    `dn-reinstall-all-activated-packages`, all built on `package.el`
    (`package-user-dir`, `package-reinstall`, `package-activated-list`). The
    config moved to straight; these can't work. The "essentials" list also
    names `counsel`, `ivy`, `lsp-ivy`, which are gone. `init-package.el`
    keeps the commented-out MELPA/`package-install` bootstrap.

17. **Ivy remnants.** `all-the-icons-ivy` (`init-misc.el:79`) is installed
    although ivy is gone; `all-the-icons` is installed while dashboard uses
    `nerd-icons`.

18. **The same package configured twice:**
    - `nix-mode`: `init-programming.el:235` and `programming/nix.el:72`
    - `gitlab-ci-mode`: `init-programming.el:207` and `programming/gitlab.el`
    - `make-mode`: `programming/build-systems.el:101` and
      `programming/makefile.el`
    - `exec-path-from-shell`: two blocks in `init-env.el` differing only in
      the variable list; one block with a computed list is simpler.
    - difftastic: the `difftastic` package in `init-magit.el:240` **and** a
      hand-rolled `dn/magit-*-with-difftastic` in `programming/diff.el`
      (which is not a language, hard-`require`s magit at startup, and
      hardcodes `--background=light`). Keep one.

19. **`explain-pause-mode.el` (3,699 lines) is loaded but never enabled.**
    `init-emacs.el:86` declares it with no `:config`, `:commands` or
    `:defer`. Either drop the file or install it from its GitHub recipe on
    demand (`:commands explain-pause-mode`).

20. **`init-org.el` is disabled in `.emacs` but still tracked.** It also has
    `org-log-done` set twice, quoted lambdas (`'(lambda …)`), a nested
    `custom-set-variables` inside `:config`, and depends on an `org-directory`
    that nothing sets. Either fix and re-enable, or delete it (git keeps it).

21. **Commented-out blocks** (12 `use-package` forms, `aidermacs` being the newest, plus the irony/octave/
    eglot experiments, ~150 lines). History already preserves them; delete.

22. **Pointless `autoload` calls inside `:config`** (`build-systems.el:114`,
    `matlab.el:45-46`, `pov-ray.el:41`). By the time `:config` runs the
    package is loaded, so the autoload is a no-op.

23. **`(use-package use-package :straight t)`** in `init-package.el`.
    `use-package` is built into Emacs 29+; straight will clone the
    upstream repo and shadow the built-in. Also `straight-use-package 'org`
    appears in both `init-package.el` and `init-org.el`.

24. **`emojify`** is fully configured (`init-misc.el:53-74`) but its
    global mode is commented out, so it's ~30 lines of inert config.

25. **`init-magit.el`:** `(make-local-variable 'split-height-threshold)` at
    load time makes the variable buffer-local in whatever buffer happens to
    be current, which is not what was intended. `point-at-eol`/
    `point-at-bol` are obsolete (`pos-eol`/`pos-bol` or
    `line-end-position`). `conv-commit-type-prompt` calls the private
    `consult--read`.

26. **`programming/cpp.el`:** `c-basic-indent` is set both in `:custom` and
    in a later `custom-set-variables`, and that same `custom-set-variables`
    sets `indent-tabs-mode nil` globally from inside a C++ module.
    `flycheck-clang-tidy` declares `:functions flycheck-clang-analyzer-setup`
    (copy-paste from the block above).

27. **`programming/web.el` installs `company-web`** and pushes to
    `company-backends`, but company is not in the config (completion is
    `:capf`). `(use-package json :straight t)` in the same file is a
    built-in library, and `json.el` separately declares `json-ts-mode
    :straight t`, another built-in.

28. **`programming/build-systems.el`** adds a global `before-save-hook` for
    PKGBUILD (guarded by `major-mode`); a mode-local hook is cleaner.

### C. Consistency and hygiene

29. **File headers, `provide` and "ends here" footers are out of sync** in
    most language modules. Concretely:

    | File | Header says | Provides | Footer says |
    |---|---|---|---|
    | `programming/json.el` | "C++ support" | `init-prog-json` | `cpp.el ends here` |
    | `programming/terraform.el` | "C++ support" | `init-prog-terraform` | `cpp.el ends here` |
    | `programming/windows.el` | "C++ support" | `init-prog-windows` | `windows.el ends here` |
    | `programming/yaml.el` | "C++ support" | `init-prog-yaml` | ok |
    | `programming/makefile.el` | "Initialisation for Python" | `init-prog-makefile` | `init-prog-build-systems.el` |
    | `programming/python.el` | ok | `init-prog-build-systems` | `init-prog-build-systems.el` |
    | `programming/web.el` | "MATLAB/Octave" | `init-prog-web` | ok |
    | `programming/gnuplot.el` | "MATLAB/Octave" | ok | ok |
    | `programming/robotframework.el` | "ROS2 support", file name `robotframework.el.el` | `robotframework.el` | `robotframework.el.el` |
    | `programming/ros.el` | `init-ros2.el` | `init-ros2` | `init-ros2.el` |
    | `programming/cpp.el` | ok | `cpp` (no prefix) | ok |
    | `programming/diff.el`, `nix.el` | no header at all | nothing | none |
    | `init-docs.el` | "Initialisation for programming" | ok | ok |
    | `config/variables.el` | header starts with `;;` not `;;;` | ok | ok |

    Four different feature-name conventions coexist (`init-prog-x`,
    `init-x`, `x`, `x.el`). This is exactly what a batch byte-compile in CI
    would catch (see Part 2, phase 0).

30. **`Local Variables` footers with `eval:` forms** are copy-pasted into
    13 files. They `setq` the global `config-dotemacs-lisp` and `config-dir`
    whenever the file is *visited*, trigger "unsafe local variable"
    prompts, and are wrong in `programming/*` (they compute `config/`
    relative to `programming/`). Replace with a single `.dir-locals.el` at
    the repo root, or with a proper load-path so flymake/flycheck can find
    `config-functions`.

31. **Namespace.** Personal symbols use `dn-`, `dn/`, `dn--`, `my-`, `pd--`,
    `conv-commit-`, `yas-lib-`, or no prefix at all (`en-abb`, `fr-abb`,
    `imdoc`, `crm-indicator`, `revert-all-buffers`, `kill-from-line-beginning`,
    `shutdown-emacs-server`, `magit-push-to-all-remotes`,
    `magit-run-mergiraf-*`, `add-conventional-commit-faces`,
    `bury-compile-buffer-if-successful`, `treesit-install-all-grammars`,
    `text-mode-hook-setup`, `lsp-update-server`, and the five `lsp-cleanup-*`/`lsp-list-workspaces` commands in `cleanup-lsp-workspaces.el`). Unprefixed `magit-*`,
    `lsp-*`, `treesit-*` and `smerge-*` names risk clashing with upstream.
    Pick one prefix (`dn-`) and one customization group (`dn`, which is
    referenced by six `defcustom`s but never `defgroup`ed).

32. **Built-in libraries declared inconsistently.** `cc-mode`, `python`,
    `make-mode`, `rst`, `lsp-nix`, `vertico-*` correctly use `:straight nil`;
    `epg`, `diff-mode`, `display-line-numbers`, `printing`, `json`,
    `json-ts-mode`, `savehist`, `use-package` do not.

33. **Formatting.** 13 files mix tabs and spaces; closing parens are
    routinely on their own line; `(if x (progn …))` instead of `when`;
    `'(lambda …)` instead of `#'`/`lambda`; `(progn …)` as the sole body of
    `:config`. A one-time pass with `indent-region` under
    `indent-tabs-mode nil` (enforced through `.dir-locals.el`) plus
    `checkdoc` would settle this.

34. **Host-specific artifacts in the repo.** `emacs.service` hardcodes
    `/snap/bin/emacs` and `LD_LIBRARY_PATH=/usr/local/lib` with commented
    alternatives; the four `docsets/*.tgz` are Git LFS blobs (not
    fetchable in this environment) that `dn-dash-docs-install` re-installs
    from. Given the Nix/home-manager docsets, the service unit and docset
    generation probably belong in the home-manager config, with this repo
    referencing them by path.

35. **`.emacs` trailing statement.** `(put 'narrow-to-region 'disabled nil)`
    sits after the `;;; .emacs ends here` footer.

36. **README gaps.** It does not mention that `config-require` looks in
    `.emacs_lisp/config/` *first* (`functions.el:82-87`), so a gitignored
    `config/init-magit.el` silently replaces the tracked module. That is a
    useful override hook but it is undocumented and easy to trip over. The
    README also does not describe `yas-lib/`, `packages/`, `abbrev_*`, or
    which Emacs version is required (the config assumes 29+: `treesit`,
    `setopt`, built-in `use-package`).

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
   `explain-pause-mode.el`, `hl-line+.el`, `ris.el`, `project-directory.el`,
   `cmake-format.el` (all third-party) sit next to `init-*.el`,
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
   side effect of the `lsp-mode` block. There is no `early-init.el`.

8. **Nothing verifies the config.** No batch load, no byte-compile, no CI.
   Items 1, 3, 7, 9, 29 in Part 1 would all have been caught by
   `emacs --batch -l init.el` plus `byte-compile-file` with warnings on.

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
│   ├── vendor/              pristine third-party: explain-pause-mode, hl-line+, ris, project-directory, cmake-format
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
| 0. Safety net | `Makefile` + CI batch load; `.dir-locals.el`; fix the provide/header table (item 29); remove `Local Variables` footers. | none |
| 1. Delete | Items 16-24, 27: package.el functions, ivy/company remnants, commented blocks, duplicates, `explain-pause-mode`, decide on `init-org.el`. | none |
| 2. Fix bugs | Items 1-15 (python `:dash`, ispell args, `dn-cmd-after-saved-file`, `lsp-nix` ordering, keybinding clash, `.yas-parents`, network at startup, `config-require` leak). | low |
| 3. Re-cut modules | Split `init-emacs.el` into `ui`/`completion`/`editing`; move tooling out of `init-programming.el` into `lsp.el`; move language packages into `lang/`; create `os-darwin.el`; `lib/` + `vendor/` + `patches/` split. Pure moves, no behaviour change. | low |
| 4. Loader + naming | `dn-` prefix everywhere; `defgroup dn`; single `dn-load-directory`; `dn-modules`/`dn-disabled-languages`; README rewrite. | medium |
| 5. Init directory | `early-init.el` + `init.el`, `--init-directory` support, custom-file ordering fix, `straight-use-package-by-default`, defer audit. | medium |

Phases 0-2 are worth doing regardless of whether the restructuring in 3-5
is adopted.

### Quick wins (under an hour total)

- Delete `snippets/cmake-mode/.yas-parents`.
- Remove `:dash "Python 3" "NumPy" "SciPy"` and merge the two `:config`
  sections in `programming/python.el`; fix its `provide`.
- Delete `(setq-default ispell-extra-args '("--reverse"))` in
  `init-ispell.el`.
- Delete `init-custom-functions.el:51-98` (package.el helpers).
- Delete `all-the-icons-ivy`, `magit-popup`, `company-web`, the second
  `nix-mode`/`gitlab-ci-mode`/`make-mode` blocks, and
  `programming/diff.el` or the `difftastic` block.
- Move `(devdocs-update-all)` into `dn-devdocs-install`.
- Add `(defgroup dn nil "Personal configuration." :group 'emacs)` to
  `config/variables.el`.
- Move the copilot-chat chords in `init-llm.el` into their own prefix map.
- Add `:straight t` to `consult`; `:straight nil` to the built-ins in
  item 32.
