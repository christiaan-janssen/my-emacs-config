;;; init.el --- Emacs Configuration --- init  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:


;; Clean up ui
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Setup package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)


;; UI and theme settings
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
  (set-face-attribute 'default nil :font "Fira Code" :height 140))
;;  (set-face-attribute 'default nil :font "JetBrainsMono" :height 120)

(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
  (load-theme 'catppuccin :no-confirm))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(setopt tab-always-indent 'complete)

(use-package company
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

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

(use-package lsp-ui)

(use-package elpy)

(use-package slime
  :config
  (slime-setup '(slime-fancy
	   slime-quicklisp
	   slime-asdf
	   slime-mrepl)))

(use-package slime-repl-ansi-color)
(setq inferior-lisp-program "sbcl")

(use-package magit
  :bind ("C-c C-g" . magit))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-directory (file-truename "~/org/roam")))

(setq org-agenda-files '("~/org/"))

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-agenda-custom-commands
    '(("A" "Active Tags" tags "rabo"
       ((org-agenda-overriding-header "My Active items")
        (org-tags-match-list-sublevels t)
        (org-agenda-prefix-format "  %?-12t% s")))))

(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/Outlook/Drafts")
  (setq mu4e-sent-folder   "/Outlook/Sent")
  (setq mu4e-trash-folder  "/Outlook/Trash"))

(defmacro setup-lang (lang extensions)
  "Set up LANG-ts-mode using `use-package` and associate it with EXTENSIONS.

LANG should be a symbol like `json` or `yaml`.
EXTENSIONS should be a list of strings like (\"json\" \"jsonc\").

This macro expands to a `use-package` declaration that associates
file extensions with the corresponding tree-sitter mode."
  `(use-package ,(intern (format "%s-ts-mode" lang))
     :ensure t
     :mode ,(mapcar (lambda (ext)
                      (format "\\.%s\\'" ext))
                    extensions)))

;;  (setup-lang "elixir" ("ex" "exs" "mix.lock"))
;;  (setup-lang "python" ("py"))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package which-key
  :defer t
  :diminish t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode))

(use-package scad-mode)
