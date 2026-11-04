lambda-x
========

Emacs extensions and configuration.

GNU Emacs 30.1 or newer is required, including built-in `use-package` support
for the `:vc` keyword. Put this in file ~/.emacs.d/init.el:

    ;;; -*- lexical-binding: t -*-
    (load "path-to-<lambda-init.el>")

Add `;;; -*- lexical-binding: t -*-` as the first line of your init file.
The configuration modules use lexical binding. Vendored legacy libraries
that have not been migrated explicitly retain dynamic binding with
`lexical-binding: nil`.

JSON files use `json-ts-mode` when the JSON tree-sitter grammar is available,
and otherwise fall back to `json-mode`. Install the grammar with
`M-x treesit-install-language-grammar RET json RET` to enable tree-sitter.

Both `go-mode` and `go-ts-mode` enable Eglot and format and organize imports
on save when a language server is connected. The configured server executable,
`trae-gopls` or `gopls`, must be available in Emacs's `exec-path`.

GUI, terminal and daemon sessions
--------------------------------

Fonts are configured per GUI frame, including new emacsclient frames. Set
`LAMBDA_EMACS_FONT` to override the default font. Terminal fonts belong to the
terminal emulator. macOS imports PATH, GOPATH and GOBIN from the login shell once,
including daemon startup.

Emacs 31 uses native terminal child frames for Corfu. Emacs 30 installs the
maintained `corfu-terminal` package and its dependencies through package.el.
Long lines wrap visually by default, including in side-by-side Ediff windows;
this does not insert newlines into files. Modes that explicitly enable truncation
can still override the default; use `M-x toggle-truncate-lines` in those buffers.

Copying with Evil `y`/`yy` or `M-w` in terminal frames uses Clipetty's OSC 52
integration, which requires support in the terminal and multiplexer.
Apply `misc/_tmux.conf` to enable extended keys; ordinary prefixes remain available:
`C-c e d` for Embark DWIM, `C-c e .` for Embark actions,
and `C-c /` for Cape completion commands. `C-;` is the perspective prefix.

For Alacritty -> SSH -> Linux tmux -> Emacs, configure clipboard forwarding
on the Linux host as shown below. Alacritty's local `[terminal]` setting
`osc52` must allow copying (`"OnlyCopy"` or `"CopyPaste"`); the supplied
`misc/alacritty.toml` already uses `"CopyPaste"`.

To persist these settings, add the following to `~/.tmux.conf` on the Linux host:

```tmux
set -s set-clipboard on
set -as terminal-features ',alacritty*:clipboard,xterm*:clipboard'
set -ag update-environment 'SSH_TTY'
```

Reload the file in the existing tmux server without ending sessions:

```sh
tmux source-file ~/.tmux.conf
tmux show -s set-clipboard
```

The last command should print `set-clipboard on`. New tmux servers read the
file automatically.

Detach and reattach from the current SSH connection to refresh the session's
`SSH_TTY`. Clipetty normally writes directly to that SSH terminal, bypassing
remote tmux; a stale `SSH_TTY` can still break copying even when tmux's clipboard
settings are correct. Inside Emacs, enable `M-x global-clipetty-mode` if needed
(use a positive prefix argument, `C-u 1`, to enable rather than toggle it).
Copy with Evil `y`/`yy` or `M-w`, then paste into a local application using the
system paste shortcut. These Emacs settings do not configure a separate Vim
process, and reading the local clipboard with Evil `p` over SSH is a separate
operation.

Perspective owns session restoration. Desktop restoration is no longer enabled;
existing Desktop save files are preserved. Dired uses text-only subtree expansion,
while Treemacs uses its built-in theme for GUI and terminal frames.
Backups remain disabled; buffer autosaves live under the Emacs cache directory.

Tree-sitter grammars are installed explicitly with
`M-x treesit-install-language-grammar`; opening files does not trigger installation.
C/C++, Java, Go, Python and TypeScript configure classic and tree-sitter modes.
Strict Smartparens editing is limited to Lisp-family modes.

Set `LAMBDA_GOPLS` to a Go language-server executable. Otherwise `trae-gopls` is
preferred when present, with `gopls` as the fallback. Set `LAMBDA_SQL_DIALECT` for
a default SQLFluff dialect (otherwise ANSI); projects can safely override the
buffer-local `flymake-sqlfluff-dialect` string in `.dir-locals.el`. SQL diagnostics
are enabled only when SQLFluff is installed.

JSON Lines uses a separate mode; `C-c C-f` validates every record before compacting
it to one line. Org PDF export uses latexmk with XeLaTeX when available, otherwise
two XeLaTeX passes. Custom LaTeX classes such as `org-article` must be installed
separately on each machine.

Configuration ownership
-----------------------

`lambda-core.el` owns shared editor behavior. `lambda-vertico.el` owns minibuffer
completion, including Marginalia and Embark; `lambda-emacs-lisp.el` owns Elisp
Flymake and package linting. Language modules own their mode-specific hooks.
Eglot, Flymake, Python, Which-key and Windmove use the built-in libraries;
external language servers are still installed separately.

The default module list retains the existing language support and interactive
commands. Ivy, Company and DAP modules are already disabled and have no startup
cost. Tern, FZF, SVN integration and the Evil commenting packages remain enabled
for compatibility with existing workflows; remove them only if those commands
are unused. Changing `lambda-libraries` can skip language modules, but keep
`lambda-session` last and account for modules loaded through `require`.

`lambda-custom.el` is still loaded for saved Custom values. Its package selection
list may contain historical packages; do not treat it as a reliable list of the
packages used by the current configuration or automatically delete packages from
that list.
