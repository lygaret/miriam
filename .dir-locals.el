((miriam-scheme-mode
  . ((indent-tabs-mode . nil)))
 (nil
  . ((eval
      . (progn
          ;; trim whitespace
          (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

          ;; put the project path in various useful places
          (let ((localsdir (locate-dominating-file default-directory ".dir-locals.el")))
            (when localsdir
              (let* ((localspath (expand-file-name localsdir))
                     (rootdir    (directory-file-name localspath))
                     (scmdir     )
                     (emacsdir   ))

                ;; add `emacs` to the the emacs load-path
                (add-to-list
                 'load-path
                 (directory-file-name (expand-file-name "./emacs" rootdir)))

                ;; add `scm` to the geiser load-path (guile)
                (add-to-list
                 'geiser-guile-load-path
                 (directory-file-name (expand-file-name "./scm" rootdir)))

                ;; ensure that .ssm files are handled with miriam mode
                (require 'miriam-scheme)
                (add-to-list 'auto-mode-alist '("\\.s\\(?:[cs]m\\|ld\\)\\'" . miriam-scheme-mode))))))))))
