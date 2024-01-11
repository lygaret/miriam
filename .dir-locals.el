;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
                   (let
                       ((localsdir
                         (locate-dominating-file default-directory ".dir-locals.el")))
                     (when localsdir
                       (let*
                           ((localspath
                             (expand-file-name localsdir))
                            (rootdir
                             (directory-file-name localspath))
                            (scmdir)
                            (emacsdir))
                         (add-to-list 'load-path
                                      (directory-file-name
                                       (expand-file-name "./emacs" rootdir)))
                         (add-to-list 'geiser-guile-load-path
                                      (directory-file-name
                                       (expand-file-name "./scm" rootdir)))
                         (require 'miriam-scheme)
                         (add-to-list 'auto-mode-alist
                                      '("\\.s\\(?:[cs]m\\|ld\\)\\'" . miriam-scheme-mode)))))))))
 (miriam-scheme-mode . ((indent-tabs-mode . nil))))
