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

;;; Code:

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

 (provide 'init)
 ;;; init.el ends here
