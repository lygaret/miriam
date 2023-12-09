((nil
  . ((eval . (let ((localsdir (locate-dominating-file default-directory ".dir-locals.el")))
               (when localsdir
                 (let* ((localspath (expand-file-name localsdir))
                        (rootdir    (directory-file-name localspath))
                        (emacsdir   (expand-file-name "./emacs" rootdir)))

                   ;; add `emacs` to the the emacs load-path
                   (add-to-list 'load-path emacsdir)
                   (require 'miriam-scheme)))))
     
     ;; automatically set the geiser load-path for guile
     (eval . (with-eval-after-load 'geiser-guile
               (let ((localsdir (locate-dominating-file default-directory ".dir-locals.el")))
                 (when localsdir
                   (let* ((localspath (expand-file-name localsdir))
                          (rootdir    (directory-file-name localspath))
                          (scmdir     (expand-file-name "./scm" rootdir)))
                     (add-to-list 'geiser-guile-load-path scmdir))))))))

 (miriam-scheme-mode
  . ((indent-tabs-mode . nil))))
