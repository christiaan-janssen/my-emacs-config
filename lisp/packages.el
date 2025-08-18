;;; init.el --- Emacs Configuration --- Packages  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:


;;; Company mode
(use-package company
  :straight t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;;; Org-Mode

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package vterm
  :straight t)

;;; Git Tools
(use-package magit
  :straight t)

(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
	 (elixir-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t)

(use-package elpy
  :straight t)


(provide 'packages)
;;; packages.el ends here
