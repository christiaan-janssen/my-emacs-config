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
;;  An Emacs config with as little external packages as possible.

(setq inhibit-statup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

(set-face-attribute 'default nil :font "JetBrainsMono" :height 120)

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

(use-package corfu
  :straight t
  :init
  (global-corfu-mode))

(use-package which-key
  :defer t
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
