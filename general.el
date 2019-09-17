;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is a general set-up that should apply to all sessions

;; Package archives
;; Out of China, this is so far the best mirrors
;; Within China, we should replace these with TsingHua's mirrors
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq backup-directory-alist `(("." . "~/.emacs_save")))

(load-theme 'dracula t)

;; icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; neotree
(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; start up size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106) ; chars
              (height . 60) ; lines
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106)
              (height . 60)
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))

;; Hack font
;; Click [here](https://github.com/hbin/dotfiles-for-emacs) to take a further look.
(set-frame-font "Hack:pixelsize=12")

;; eshell (are we ready to fully dump eshell?)
;; (defalias 'o 'find-file)
;; (defalias 'oo 'find-file-other-window)
;; (setq eshell-scroll-to-bottom-on-input t)

;; (setq explicit-shell-file-name "/usr/bin/fish")
(require 'use-package)
(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/local/bin/fish")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; General Settings
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 6)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq debug-on-error t)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq mouse-yank-at-point t)
(setq ediff-diff-options "-w")
(setq-default ediff-ignore-similar-regions t)
(setq-default ediff-highlight-all-diffs nil)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(global-visual-line-mode t) ; Do not cut words
(show-paren-mode t) ;; highlight the parenthesis
(tool-bar-mode 0) ;; no tool bar
(customize-set-variable 'scroll-bar-mode 'right)
(global-linum-mode t)
(auto-image-file-mode t)
(setq frame-title-format "%b")
(global-set-key [(home)] 'beginning-of-buffer) ;; set home key to the beginning of buffer
(global-set-key [(end)] 'end-of-buffer) ;; set end key to the end of buffer
(minibuffer-electric-default-mode 1) ;; enable minibuffer (should be by default)
(icomplete-mode 1) ;; auto comple in minibuffer
(fset 'yes-or-no-p 'y-or-n-p)
(setq resize-mini-windows t)
(setq uniquify-buffer-name-style 'forward)
(setq Man-notify-method 'pushy)
(mouse-avoidance-mode 'animate)
(auto-compression-mode 1)
(column-number-mode t)
(blink-cursor-mode -1)
(transient-mark-mode 1)
(show-paren-mode 1)
(mouse-wheel-mode t)
(setq scroll-step 1
scroll-margin 3
scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq track-eol t)
(setq-default kill-whole-line t)
(setq kill-ring-max 200)
(setq apropos-do-all t)
(setq-default ispell-program-name "aspell")
(savehist-mode 1)
(put 'narrow-to-region 'disabled nil)
;; set the clipboard
(setq x-select-enable-clipboard t)
(if (eq system-type 'gnu-linux) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
;;; (modern-c++-font-lock-global-mode t) ;; modern-c look and feel
(global-set-key (kbd "C-c l") 'global-hl-line-mode) ;; toggle highlight the current line
(global-set-key (kbd "C-c c p") 'company-mode) ;; toggle company-mode

;;
;; auto completion related features
;;

;; 1 - simple meta \ completion
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
'(try-expand-line
try-expand-line-all-buffers
try-expand-list
try-expand-list-all-buffers
try-expand-dabbrev
try-expand-dabbrev-visible
try-expand-dabbrev-all-buffers
try-expand-dabbrev-from-kill
try-complete-file-name
try-complete-file-name-partially
try-complete-lisp-symbol
try-complete-lisp-symbol-partially
try-expand-whole-kill))

;; 2 - Helm
(require 'helm-config)
(require 'helm-ag)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-y") 'helm-do-ag)
(global-set-key (kbd "C-x f") 'helm-ag-this-file)
; (global-set-key (kbd "M-x") #'helm-M-x) ; a bit too much I think
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-mode 1)

;; 3 - TabNine: AI based completion
;; this requires clang to be installed:
;; sudo apt-get install clang-7 lldb-7 lld-7 --fix-missing
;; sudo ln -s /usr/bin/clang-7 /usr/bin/clang
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
(setq company-idle-delay 0) ;; Trigger completion immediately.
(setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
(add-hook 'gud-gdb-mode-hook (lambda() (company-mode 0))) ;; Do not use company-mode in gud-gdb mode
(add-hook 'wl-summary-mode-hook (lambda() (company-mode 0))) ;; too slow to have this on
;; 
;; Other utils
;;

;; search string in all opened buffers (C-x g). I know that string is in my Emacs somewhere! 
(require 'cl)
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)
(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))
   regexp))
(global-set-key (kbd "C-x g") 'search-all-buffers)
;; (global-set-key (kbd "C-*") 'evil-search-word-forward)
;; (global-set-key (kbd "C-#") 'evil-search-word-backward)

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "C-x y") 'my-put-file-name-on-clipboard)

;;
;; Some old settings that need to be cleaned up
;;
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; HTML stuff
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(eval-after-load 'hideshow
 '(progn
   (global-set-key (kbd "C-+") 'hs-toggle-hiding)))
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
