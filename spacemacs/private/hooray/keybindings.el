;; Use C-x C-e to evaluate region
(global-set-key (kbd "C-x C-e") 'eval-region)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-x F") 'find-file-as-root)

;; bind RET to newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; translate C-i to ^, which will be useful in latex. ( Originally C-i is same as tab )
(keyboard-translate ?\C-i ?^ )

