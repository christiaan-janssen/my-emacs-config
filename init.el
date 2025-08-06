;;; init.el --- Emacs Configuration --- Init  -*- lexical-binding: t; -*-
;;
;; Author: Christiaan Janssen
;; URL:
;; Package-Requires:
;; Keywords: Config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;;  After being inspired by Emacs-Solo, I'm trying something like that.
;;  An Emacs config with a minimum of external packages as possible.

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

(set-face-attribute 'default nil :font "JetBrainsMono" :height 140)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'utils)

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

(straight-use-package 'use-package)


(use-package general
  :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; Enable Vertico.
(use-package vertico
  :straight t
;;  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(setopt tab-always-indent 'complete)

(use-package corfu
  :straight t
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  :init
  (global-corfu-mode))

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Org-Mode

(use-package org-roam
  :straight t
  :config
  (setq org-roam-directory (file-truename "~/org-roam")))

;;; Setup Language support

;; List with Treesitter language grammars
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

;; Use eglot to connect with LSP
(require 'eglot)

(setup-lang "elixir" ("ex" "exs" "mix.lock"))

;; Automatically run `M-x eglot` for `elixir-mode`:
(add-hook 'elixir-mode-hook 'eglot-ensure)
;; Add LSP server path
(add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/lsp/elixir-ls/language_server.sh"))

(setup-lang "python" ("py"))

(use-package which-key
  :defer t
  :diminish t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode))

;; Borowed from Emacs-Solo:
;; https://github.com/LionyxML/emacs-solo/blob/f82486e591e0888983732c9b7ff4f948e4c021a8/init.el#L2450
(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((bg-main "#292D3E")
     (bg-active bg-main)
     (fg-main "#EEFFFF")
     (fg-active fg-main)
     (fg-mode-line-active "#A6Accd")
     (bg-mode-line-active "#232635")
     (fg-mode-line-inactive "#676E95")
     (bg-mode-line-inactive "#282c3d")
     ;; (border-mode-line-active "#676E95")
     ;; (border-mode-line-inactive bg-dim)
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (bg-tab-bar      "#242837")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#242837")
     (fg-prompt "#c792ea")
     (bg-prompt unspecified)
     (bg-hover-secondary "#676E95")
     (bg-completion "#2f447f")
     (fg-completion white)
     (bg-region "#3C435E")
     (fg-region white)

     (fg-line-number-active fg-main)
     (fg-line-number-inactive "gray50")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#292D3E")
     (fringe "#292D3E")

     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")

     (fg-prose-verbatim "#c3e88d")
     (bg-prose-block-contents "#232635")
     (fg-prose-block-delimiter "#676E95")
     (bg-prose-block-delimiter bg-prose-block-contents)

     (accent-1 "#79a8ff")

     (keyword "#89DDFF")
     (builtin "#82aaff")
     (comment "#676E95")
     (string "#c3e88d")
     (fnname "#82aaff")
     (type "#c792ea")
     (variable "#c792ea")
     (docstring "#8d92af")
     (constant "#f78c6c")))
  :config
  (modus-themes-with-colors
   (custom-set-faces
    `(tab-bar
      ((,c
        :background "#232635"
        :foreground "#A6Accd"
        ;; :box (:line-width 1 :color "#676E95")
        )))
    `(tab-bar-tab
      ((,c
        :background "#232635"
        :underline t
        ;; :box (:line-width 1 :color "#676E95")
        )))
    `(tab-bar-tab-inactive
      ((,c
        ;; :background "#232635"
        ;; :box (:line-width 1 :color "#676E95")
        )))
    `(tab-bar-tab-group-current
      ((,c
        ;; :background "#232635"
        ;; :box (:line-width 1 :color "#676E95")
        :background "#232635"
        :foreground "#A6Accd"
        :underline t
        )))
    `(tab-bar-tab-group-inactive
      ((,c
        ;; :background "#232635"
        ;; :box (:line-width 1 :color "#676E95")
        :background "#232635"
        :foreground "#777")))))
  :init
  (load-theme 'modus-vivendi-tinted t))
