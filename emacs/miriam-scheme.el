;;; miriam-scheme.el --- miriam scheme editing mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jonathan Raphaelson
;; This file is part of Miriam Scheme.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The major mode for editing miriam scheme code, based on scheme-mode,
;; distributed with Emacs.

;;; Code:

(require 'scheme)

;;;###autoload
(define-derived-mode miriam-scheme-mode scheme-mode "miriam scheme"
  "Major mode for editing miriam scheme code."
  (setq-local lisp-indent-function 'miriam-scheme-indent-function)
  (setq-local font-lock-defaults
        '((miriam-scheme-font-lock-keywords
           miriam-scheme-font-lock-keywords-1
           miriam-scheme-font-lock-keywords-2)
          nil ; keywords only
          t   ; cose-fold (insensitive)
          (("+-*/.<>=!?$%_&~^:" . "w")  ; syntax table additions (change word chars)
           (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(defvar miriam-scheme-font-lock-keywords
  `(("(\\(pseudo\\_>\\)" (1 font-lock-keyword-face))
    ("(\\(block\\_>\\)" (1 font-lock-keyword-face))
    ("(\\(scope\\_>\\)" (1 font-lock-keyword-face))
    ,@scheme-font-lock-keywords))

(defvar miriam-scheme-font-lock-keywords-1
  `(("(\\(pseudo\\_>\\)\\s-*(\\(\\sw+\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("(\\(block\\_>\\)\\s-*\\(\\sw+\\)"   (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("(\\(scope\\_>\\)"                   (1 font-lock-keyword-face))
    ,@scheme-font-lock-keywords-1))

(defvar miriam-scheme-font-lock-keywords-2
  `(("(\\(pseudo\\_>\\)\\s-*(\\(\\sw+\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("(\\(block\\_>\\)\\s-*\\(\\sw+\\)"   (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("(\\(scope\\_>\\)"                   (1 font-lock-keyword-face))
    ("\\_<\\?\\(?:al\\|c[cs]\\|eq\\|g[et]\\|hi\\|l[est]\\|mi\\|ne\\|pl\\|v[cs]\\)\\_>" . font-lock-builtin-face)
    ("\\_<\\(?:fb\\|ip\\|lr\\|pc\\|r\\(?:1[0-5]\\|[0-9]\\)\\|s[bl]\\)\\_>" . font-lock-variable-name-face)
    ,@scheme-font-lock-keywords-2))

(put 'pseudo 'miriam-scheme-indent-function 1)
(put 'block  'miriam-scheme-indent-function 2)
(put 'scope  'miriam-scheme-indent-function 0)

;; FIXME this duplicates almost all of scheme-indent-function.
;; Extract common code to a subroutine.
(defun miriam-scheme-indent-function (indent-point state)
  "Miriam scheme mode function for the value of the variable `scheme-indent-function'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point)) calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point) (progn (forward-sexp 1) (point)))) method)
        (setq method (or
                      (get (intern-soft function) 'miriam-scheme-indent-function)
                      (get (intern-soft function) 'scheme-indent-function)
                      (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(provide 'miriam-scheme)

;;; miriam-scheme.el ends here
