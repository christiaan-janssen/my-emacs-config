# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Emacs configuration inspired by Emacs-Solo, aiming for a minimal setup with as few external packages as possible. The configuration uses straight.el for package management and focuses on modern Emacs features like tree-sitter for syntax highlighting.

## Architecture

### Core Files
- `init.el` - Main configuration entry point with UI setup, theme configuration, and language support
- `lisp/utils.el` - Utility functions and macros (notably `setup-lang` for tree-sitter mode setup)
- `lisp/packages.el` - Package declarations using straight.el and use-package

### Package Management
The configuration uses **straight.el** as the package manager with **use-package** for configuration. All packages are declared in `lisp/packages.el` and loaded via `(require 'packages)` in `init.el`.

### Language Support Architecture
Languages are configured using the `setup-lang` macro from `utils.el`:
```elisp
(setup-lang "python" ("py"))
(setup-lang "elixir" ("ex" "exs" "mix.lock"))
```

Tree-sitter grammars are defined in `treesit-language-source-alist` in `init.el`.

### LSP Integration
- Uses **eglot** (built-in LSP client) instead of lsp-mode
- Language servers are configured in `eglot-server-programs`
- Example: Elixir LSP server path: `~/.emacs.d/lsp/elixir-ls/language_server.sh`

## Key Packages and Features

### Completion Framework
- **Vertico**: Minibuffer completion interface
- **Marginalia**: Rich annotations for completions
- **Orderless**: Flexible completion style
- **Corfu**: In-buffer completion popup
- **Kind-icon**: Icons for completion candidates

### Theme and UI
- **Modus themes**: Uses `modus-vivendi-tinted` with extensive custom color overrides
- Custom Material Theme-inspired color palette defined in `modus-themes-common-palette-overrides`
- UI elements disabled: startup message, scroll bar, tool bar, tooltip, menu bar

### Development Tools
- **claude-code-ide**: Integration with Claude Code CLI
- **org-roam**: Note-taking system (directory: `~/org-roam`)
- **exec-path-from-shell**: Environment variable synchronization

### Git Integration
- **Magit**: Complete Git interface with keybindings:
  - `C-x g`: magit-status (main Git interface)
  - `C-x M-g`: magit-dispatch (command menu)
  - `C-c M-g`: magit-file-popup (file-specific actions)
- **diff-hl**: Modern git gutter showing changed lines in fringe
  - Integrates with Magit for automatic refresh
  - Shows added/modified/deleted lines with visual indicators

## Adding New Languages

To add support for a new language:

1. Add the tree-sitter grammar to `treesit-language-source-alist` in `init.el`
2. Use the `setup-lang` macro in `init.el`:
   ```elisp
   (setup-lang "language-name" ("ext1" "ext2"))
   ```
3. Configure LSP server if needed:
   ```elisp
   (add-to-list 'eglot-server-programs '(language-mode "path/to/language-server"))
   (add-hook 'language-mode-hook 'eglot-ensure)
   ```

## Configuration Patterns

### Package Declaration
All packages use this pattern in `lisp/packages.el`:
```elisp
(use-package package-name
  :straight t  ; or custom recipe
  :config
  ;; configuration here
  )
```

### Tree-sitter Mode Setup
Use the `setup-lang` utility macro rather than manual file associations:
```elisp
(setup-lang "json" ("json" "jsonc"))
```

## File Organization

```
.emacs.d/
├── init.el                 # Main configuration
├── lisp/
│   ├── utils.el           # Utility functions and macros
│   └── packages.el        # Package declarations
├── lsp/                   # Language server installations
│   └── elixir-ls/
└── straight/              # Package installation directory
```

## Development Commands

When working on this configuration, note that:
- Configuration changes in `lisp/` files require `M-x load-file` or restart
- Tree-sitter grammars may need installation: `M-x treesit-install-language-grammar`
- LSP servers must be installed separately and paths updated in configuration