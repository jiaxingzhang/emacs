;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is for Bible related settings

(add-to-list 'load-path "~/.emacs.rc/local")

;; Look up CPP reference website.
(defun lookup-biblegateway ()
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (eww (concat "https://www.biblegateway.com/passage/?search=" word "&version=ESV"))
    ))

(require 'esv)
;; the following keys should be mapped to whatever works best for you
; C-c e looks up a passage and displays it in a pop-up window
(define-key global-map [(control c) ?e] 'esv-passage)
;; C-c i inserts an ESV passage in plain-text format at point
(define-key global-map [(control c) ?i] 'esv-insert-passage)
;; If you don't want to use customize, you can set this for casual
;; usage (but read http://www.esvapi.org/ for license):
(setq esv-key "TEST")
