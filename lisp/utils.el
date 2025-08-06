(defmacro setup-lang (lang extensions)
  "Set up LANG-ts-mode using `use-package` and associate it with EXTENSIONS.

LANG should be a symbol like `json` or `yaml`.
EXTENSIONS should be a list of strings like (\"json\" \"jsonc\").

This macro expands to a `use-package` declaration that associates
file extensions with the corresponding tree-sitter mode."
  `(use-package ,(intern (format "%s-ts-mode" lang))
     :mode ,(mapcar (lambda (ext)
                      (format "\\.%s\\'" ext))
                    extensions)))

(provide 'utils)
