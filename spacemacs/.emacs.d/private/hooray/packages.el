;;; packages.el --- hooray layer packages file for Spacemacs.
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
;; added to `hooray-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `hooray/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `hooray/pre-init-PACKAGE' and/or
;;   `hooray/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst hooray-packages
  '(
    evil
    org
    company
    helm
    yasnippet
 ;;   dired
    )
  "The list of Lisp packages required by the hooray layer.

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

(defun hooray/post-init-evil ()
  (define-key evil-motion-state-map "\C-j" (lambda () (interactive) (evil-scroll-down 0)))
  (define-key evil-motion-state-map "\C-k" (lambda () (interactive) (evil-scroll-up 0) ))

  (define-key evil-motion-state-map "f" 'evil-avy-goto-char-in-line)
  (define-key evil-motion-state-map "s" 'evil-avy-goto-symbol-1)
  (define-key evil-motion-state-map "S" 'evil-avy-goto-char-2)
  (define-key evil-motion-state-map "L" 'evil-avy-goto-line)

  (define-key evil-normal-state-map "r" 'evil-replace-state)
  (define-key evil-normal-state-map "q" 'undo-tree-redo)

  ;;  ;; navigation
  (define-key evil-normal-state-map "f" 'evil-avy-goto-char-in-line)
  (define-key evil-normal-state-map "s" 'evil-avy-goto-symbol-1)
  (define-key evil-normal-state-map "S" 'evil-avy-goto-char-2)
  (define-key evil-normal-state-map "L" 'evil-avy-goto-line)


  ;; set up evil-commandline keybindings
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "q[uit]" 'kill-this-buffer))
  (defun evil-ex-versatile-xit (&rest r)
    (interactive)
    "prevent emacs from trying to close the last window of a frame; kill buffer if the buffer is ReadOnly e.g. *Message*; allow xit without saving"
    (if (= (count-windows) 1) (progn (evil-save r t) (kill-this-buffer)) (evil-save-modified-and-close r) ))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "x[it]" 'evil-ex-versatile-xit))

  )

(defun hooray/post-init-company ()
  ;; Globally turn on company
  (global-company-mode)

  ;; TODO check if these are needed given the existence of auto-completion
  ;; ;; ;; solve conflict between company and yasnippet
  ;; (global-set-key (kbd "TAB") 'tab-indent-or-complete)
  ;; ;; (global-set-key [(control return)] 'company-complete-common)

  ;; ;; (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  ;; (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

  ;; continued in post-init-yasnippet
  )

(defun hooray/post-init-helm ()
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-quick-update                     t ; Update helm buffer without loading the out-of-screen entries
        )

  )

(defun hooray/post-init-yasnippet ()
  ;; prepend my custom snippets directory using cons. (No longer needed)
  ;; (setq yas-snippet-dirs
  ;;       ;; '("~/.emacs.d/private/snippets")
  ;;       (cons "~/.emacs.d/private/snippets" yas-snippet-dirs)
  ;;       )
  (yas-global-mode nil);; or M-x yas-reload-all if you've started YASnippet already.
  (setq yas-indent-line 'fixed)
  ;;(setq yas-also-auto-indent-first-line t)
  ;; continued from post-init-company
  ;; (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; (define-key yas-keymap [tab] 'tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)
  ;; ;; end of conflict solver

  ;; TODO fix this.
  ;; ;; put .yas-skip in yas-installed-snippets-dir/latex-mode to exclude loading this file
  ;; (let ((file (concat yas-installed-snippets-dir "/latex-mode" "/.yas-skip")))
  ;;   (when (not (file-exists-p file))
  ;;     (write-region "" nil file)))
  )

(defun hooray/post-init-org ()
  ;;Enable BaBel ob_babel_functions for some languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (python . t)))

  (require 'ob-C)
  ;; disable line-splitting of M-RET
  (setq org-M-RET-may-split-line nil)
  ;; auto display UTF-8 of greek letters and the like.
  ;; (setq org-pretty-entities t) ;;TIP: Just want to enable it in org major-mode
  (setq fill-column 80)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-pretty-entities t)
  ;; set up org capture and agenda
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  ;; (define-key global-map "\C-cc" 'org-capture)
  ;; (define-key global-map "\C-ca" 'org-agenda)
  ;; (setq org-capture-templates
  ;;       '(("k" "knowledge" entry (file+headline "~/org/capture.org" "Knowledge")
  ;;          "** %? %(org-set-tags) \n %(spaces-string 1) %u") ;%(spaces-string 1) is for alignment
  ;;         ("t" "task" entry (file+headline "~/org/capture.org" "Tasks")
  ;;          "** TODO %? %(org-set-tags) \n %(spaces-string 1) %t")
  ;;         ("q" "questions" entry (file+headline "~/org/capture.org" "Questions")
  ;;          "** TODO %? %(org-set-tags) \n %(spaces-string 1) %t")
  ;;         ("c" "c++ template" entry (file+headline "~/org/capture.org" "C++ Template")
  ;;          "** %? %(org-set-tags) \n %(spaces-string 1) %u")
  ;;         ))
  )

;; (defun hooray/init-dired ()
;;   ;; set default behavior when calling ! or & on files
;;   ;; Note the -user overrules -default
;;   (setq dired-guess-shell-alist-user
;;         '(("\\.pdf\\'" "evince")
;;           ))
;;   )

;;; packages.el ends here
