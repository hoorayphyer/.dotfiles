;;; packages.el --- c++ide layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <hooray@x1carbon>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `c++ide-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `c++ide/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `c++ide/pre-init-PACKAGE' and/or
;;   `c++ide/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst c++ide-packages
  '(
    cc-mode
    company
    company-irony
    company-irony-c-headers
    cmake-mode
    irony
    flycheck
    ;; rtags
    )
  "The list of Lisp packages required by the c++ide layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; (defun c++ide/init-ccmode ()
;;   (use-package ccmode))

;; the following is copied from c-c++ layer.
(defun c++ide/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . c++-mode))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "a" 'projectile-find-other-file
        "A" 'projectile-find-other-file-other-window
        ;; "r" 'projectile-recentf
        ;; "c" 'projectile-compile-project
        ;; "f" 'helm-projectile-find-file
        )
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "a" 'projectile-find-other-file
        "A" 'projectile-find-other-file-other-window
        ;; "r" 'projectile-recentf
        ;; "c" 'projectile-compile-project
        ;; "f" 'helm-projectile-find-file
        )
      ;; define my own compilation rules and bind it to f5
      (defun hooray/compile ()
        (interactive)
        ;; first save all buffers of the project
        (projectile-save-project-buffers)
        ;; then run compilation
        (let ((root-dir (projectile-project-root)))
          (projectile-run-compilation (concat "cd " root-dir "build && make"))
          )
        ;; switch to the new window. The 1 is the count of windows to skip,
        ;; starting from the current window
        (other-window 1))
      ;; bind key
      (define-key c-mode-map (kbd "<f5>") 'hooray/compile)
      (define-key c++-mode-map (kbd "<f5>") 'hooray/compile)
        )))

(defun c++ide/init-irony ()
  (use-package irony
    :commands irony-mode
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ;; use c++11 for flagging clang. NOTE this has to be done here. Using
    ;; customize turns out to be useless.
    (custom-set-variables
     '(irony-additional-clang-options (quote ("-std=c++11")))
     )

    ))

(defun c++ide/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :commands company-irony-c-headers
    :init
    ;; Load with `irony-mode` as a grouped backend
    (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

(defun c++ide/init-company-irony ()
  (use-package company-irony
    :commands company-irony
    :init
    (add-to-list 'company-backends 'company-irony)))

(defun c++ide/post-init-flycheck ()
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++11")))
  )

;;; copied from spacemacs c-c++ layers
(defun c++ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun c++ide/post-init-company ()
  (spacemacs|add-company-hook cmake-mode))

;; (defun c++ide/init-rtags ()
;;   )
;;; packages.el ends here
