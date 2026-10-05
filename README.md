lambda-x
========

Emacs extensions and configuration.

GNU Emacs 30.1 or newer is required, including built-in `use-package` support
for the `:vc` keyword. Put this in file ~/.emacs.d/init.el:

    (load "path-to-<lambda-init.el>")

JSON files use `json-ts-mode` when the JSON tree-sitter grammar is available,
and otherwise fall back to `json-mode`. Install the grammar with
`M-x treesit-install-language-grammar RET json RET` to enable tree-sitter.

Both `go-mode` and `go-ts-mode` enable Eglot and format and organize imports
on save when a language server is connected. The configured server executable,
`trae-gopls`, must be available in Emacs's `exec-path`.
