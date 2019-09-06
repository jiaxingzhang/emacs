;;
;; Deprecated stuff
;;

;; @Deprecated@
;; mutt mode
;; (load "~/.emacs.d/mutt")
;; Mutt support: spelling
;; (add-hook 'mutt-mode-hook 'flyspell-mode)
;; (add-hook 'mutt-mode-hook 'longlines-mode)
;; (add-hook 'mutt-mode-hook 'visual-line-mode)
; open mail-mode when emacs is invoked by mutt
;; (add-to-list 'auto-mode-alist '("/mutt" . mutt-mode))
; wrap email body
; (add-hook 'mutt-mode-hook 'turn-on-auto-fill)
; (add-hook 'mutt-mode-hook
                                        ;           '(lambda() (set-fill-column 80)))

;; @Deprecated@
;; the term in emacs
;; (when (require 'term nil t) ; only if term can be loaded..
;;   (setq term-bind-key-alist
;;         (list (cons "C-c C-c" 'term-interrupt-subjob)
;;               (cons "C-p" 'previous-line)
;;               (cons "C-n" 'next-line)
;;               (cons "M-f" 'term-send-forward-word)
;;               (cons "M-b" 'term-send-backward-word)
;;               (cons "C-c C-j" 'term-line-mode)
;;               (cons "C-c C-k" 'term-char-mode)
;;               (cons "M-DEL" 'term-send-backward-kill-word)
;;               (cons "M-d" 'term-send-forward-kill-word)
;;               (cons "<C-left>" 'term-send-backward-word)
;;               (cons "<C-right>" 'term-send-forward-word)
;;               (cons "C-r" 'term-send-reverse-search-history)
;;               (cons "M-p" 'term-send-raw-meta)
;;               (cons "M-y" 'term-send-raw-meta)
;;               (cons "C-y" 'term-send-raw))))

