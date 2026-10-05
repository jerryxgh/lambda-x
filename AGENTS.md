# Repository Guide

This repository contains a modular GNU Emacs configuration for use in both GUI and terminal environments.

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

Do not generate test files or test code, including temporary test scripts outside this repository. Use the verification commands below and existing checks without adding tests.

