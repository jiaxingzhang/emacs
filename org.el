;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is for org-mode related settings

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-emphasis-alist
  '(("*" (bold :foreground "Orange" ))
    ("/" italic)
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; flyspell-mode on for org-mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; flyspell-mode to ignore in code block
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-agenda-files (list "~/notes/org/work.org"
                             "~/notes/org/home.org" 
                             "~/notes/org/ministry.org"
			     "~/notes/org/dt.org"))
