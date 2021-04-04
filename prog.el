;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is specific for programming

;; A bit TMW specific stuff: 
(setq mathworks-compile-flags "DEBUG=1")
(setq sbtools-load-p4e nil)

;; ;; I still don't like the long lines despite the wide screen
;; (setq-default header-line-format 
;;               (list " " (make-string 100 ?-) "|"))
;; This interferes with the tab. Disable for now.

(defun connect-remote ()
  (interactive)
  (dired "/jzhang@ah-jzhang-maci:~/"))

;;
;; Programming related stuff
;;
(require 'cc-mode)

;; Look up CPP reference website.
(defun lookup-cppreference ()
  "Look up the word under cursor in cppreference.
If there is a text selection (a phrase), use that.
This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (eww (concat "https://en.cppreference.com/mwiki/index.php?search=" word)) ; emacs's own browser
    ))
(define-key c++-mode-map (kbd "C-c c") #'lookup-cppreference)

(defun my-gud-gdb ()
 (interactive)
 (gud-gdb (concat "gdb --fullname "
                  (file-name-sans-extension (buffer-file-name (current-buffer))))))

;; Prevent gdb from popping i/o window to the foreground on every output op
(setq-default gdb-display-io-nopopup t)

;; Racket related stuff
(setq tab-always-indent 'complete)
