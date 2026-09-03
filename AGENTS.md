# Repository Guide

This repository contains a modular GNU Emacs configuration targeting Emacs 31.1.

## Working conventions

- Keep shared behavior in `lambda-core.el` and language-specific behavior in the matching `lambda-<language>.el` module.
- Add each runtime module to `lambda-libraries` in `lambda-init.el`; preserve dependency order and keep `lambda-session` last.
- Write code comments and documentation in English.
- Prefer `use-package` for package setup and built-in `package.el` for installation. Keep package installation out of unrelated configuration paths.
- Register mode hooks in one owning module. Make buffer-specific save hooks local by passing non-nil as the LOCAL argument to `add-hook`.
- Configure `eglot-server-programs` only after Eglot is loaded, and include both classic and tree-sitter modes where applicable.
- Treat package implementation details and double-dash symbols as unstable. Prefer public APIs or narrowly scoped advice with an explanatory comment.
- Keep machine-specific values portable through environment variables or home-relative paths.
- Preserve Emacs safeguards for file-local and directory-local variables.

## Verification

Use the Emacs 31.1 binary explicitly on this macOS workspace:

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs --batch -Q --eval '(princ emacs-version)'
```

Run `check-parens` on every changed Emacs Lisp file. When package dependencies are available, load `lambda-init.el` in batch mode with `--debug-init`. A successful change leaves `git status` free of generated `.elc` files.

Do not edit generated package contents under `packages/elpa/` or the ignored machine-local `lambda-custom.el` unless the task explicitly targets them.
