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
  ;; set up motion-state
  ;; use C-S-e to scroll up and  ?? unbind the original C-y for this job
  (define-key evil-motion-state-map (kbd "C-S-e") 'evil-scroll-line-up)
  ;; (unbind-key (kbd "C-y") evil-motion-state-map)
  ;; (unbind-key (kbd "C-y"))
  ;; set up normal-state
  (define-key evil-normal-state-map "\C-k" 'evil-end-of-line)
  (define-key evil-normal-state-map "q" 'undo-tree-redo)
  ;; set up insert-state
  (define-key evil-insert-state-map "\C-l" [right]) ;; somehow evil-forward-char doesn't go beyond the ) in the end of line.
  ;; TODO the following no longer works.
  ;; ;; rebind :wq to just save and kill current buffer
  ;; ;; Note on what is going on here
  ;; ;; 1. originally tried (evil-write) (kill-this-buffer) but it doesn't work. evil-write takes some arguments.
  ;; ;; 2. the following version copies the evil-save-and-close in evil-command.el, which makes use of the macro
  ;; ;;    evil-define-command, defined in evil-common.
  ;; ;; 3, the only difference is kill-this-buffer instead of the original evil-quit.
  ;; (use-package evil-common
  ;;   :defer t
  ;;   :config
  ;;   (evil-define-command hooray/evil-write-and-quit-buffer ( file &optional bang )
  ;;     "Saves the current buffer and closes the current buffer."
  ;;     :repeat nil
  ;;     (interactive "<f><!>")
  ;;     (evil-write nil nil nil file bang)
  ;;     (kill-this-buffer)
  ;;     ))

  ;; (use-package evil-ex
  ;;   :defer t ;; the evil package will load evil-ex, so it's deferred till then
  ;;   :config
  ;;   (evil-ex-define-cmd "wq" 'hooray/evil-write-and-quit-buffer)
  ;;   ;; TODO make :q do the following: kill this buffer. If there are multiple
  ;;   ;; windows, kill the window associated with this buffer as well.
  ;;   (evil-ex-define-cmd "q" 'kill-this-buffer)
  ;;   )
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
  (use-package helm-config
    :config
    (helm-mode 1)
    )
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

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
  (yas-global-mode 1);; or M-x yas-reload-all if you've started YASnippet already.
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

  ;; put .yas-skip in yas-installed-snippets-dir/latex-mode to exclude loading this file
  (let ((file (concat yas-installed-snippets-dir "/latex-mode" "/.yas-skip")))
    (when (not (file-exists-p file))
      (write-region "" nil file)))
  )

(defun hooray/post-init-org ()
  ;;Enable BaBel ob_babel_functions for some languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (java . t) (python . t)))

  (require 'ob-C)
  (require 'ob-java)
  ;; disable line-splitting of M-RET
  (setq org-M-RET-may-split-line nil)
  ;; auto display UTF-8 of greek letters and the like.
  ;; (setq org-pretty-entities t) ;;TIP: Just want to enable it in org major-mode
  (setq fill-column 80)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  ;; set up org capture and agenda
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-capture-templates
        '(("k" "knowledge" entry (file+headline "~/org/capture.org" "Knowledge")
           "** %? %(org-set-tags) \n %(spaces-string 1) %u") ;%(spaces-string 1) is for alignment
          ("t" "task" entry (file+headline "~/org/capture.org" "Tasks")
           "** TODO %? %(org-set-tags) \n %(spaces-string 1) %t")
          ("q" "questions" entry (file+headline "~/org/capture.org" "Questions")
           "** TODO %? %(org-set-tags) \n %(spaces-string 1) %t")
          ("c" "c++ template" entry (file+headline "~/org/capture.org" "C++ Template")
           "** %? %(org-set-tags) \n %(spaces-string 1) %u")
          ))
  )

;; (defun hooray/init-dired ()
;;   ;; set default behavior when calling ! or & on files
;;   ;; Note the -user overrules -default
;;   (setq dired-guess-shell-alist-user
;;         '(("\\.pdf\\'" "evince")
;;           ))
;;   )

;;; packages.el ends here
