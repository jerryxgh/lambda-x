lambda-x
========

Emacs extensions and configuration.

GNU Emacs 30.1 or newer is required, including built-in `use-package` support
for the `:vc` keyword. Put this in file ~/.emacs.d/init.el:

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
OSC 52 clipboard integration requires support in the terminal and multiplexer.
Apply `misc/_tmux.conf` to enable extended keys; ordinary prefixes remain available:
`C-c w` for perspectives, `C-c e d` for Embark DWIM, `C-c e .` for Embark actions,
and `C-c /` for Cape completion commands. `C-;` remains the perspective prefix.

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
