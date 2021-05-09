;; Jiaxing Zhang's GNU/Emacs file
;; Package archives
(setq package-check-signature nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")))

(setq backup-directory-alist `(("." . "~/.emacs_save")))

;; always start as a server
;; (server-start)

;; theme
;; I haven't found a good theme so far. So, let's not set theme at all.
;; (cond ((eq system-type 'darwin)
;;        (load-theme 'monokai-pro t) 
;;        (require 'powerline)       ;; this don't work well in Linux terminal
;;        (powerline-default-theme)
;;        )
;;       ((eq system-type 'gnu/linux)
;;        (load-theme 'dracula t)
;;        ))

;; icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(global-set-key (kbd "C-c q") 'remember-notes)
(global-set-key (kbd "C-c Q") 'remember-other-frame)

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
;; brew install --cask font-hack
;; brew install --cask font-fira-code
(set-frame-font "Fira Code:pixelsize=12")
(set-frame-font "Hack:pixelsize=12")

;; eshell (are we ready to fully dump eshell?)
(defalias 'o 'find-file)
(defalias 'oo 'find-file-other-window)
(setq eshell-scroll-to-bottom-on-input t)

(require 'use-package)
(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/local/bin/fish")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (setq shell-pop-autocd-to-working-dir nil) ; don't auto-cd 
  )

;; ;; General Settings
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 6)
(setq delete-old-versions t)
(setq backup-by-copying t)
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

;; (modern-c++-font-lock-global-mode t) ;; modern-c look and feel
(global-set-key (kbd "C-c l") 'global-hl-line-mode) ;; toggle highlight the current line

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

;; helm seems to interfere with gdb-gud mode? 
;; 2 - Helm 
(require 'helm-config)
(require 'helm-ag)
(require 'helm)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-y") 'helm-do-ag)
(global-set-key (kbd "C-x f") 'helm-ag-this-file)
(global-set-key (kbd "M-x") 'helm-M-x)
; (global-set-key (kbd "M-x") #'helm-M-x) ; a bit too much I think
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-mode 1)

;; ivy-mode as a replacement for helm
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-height 20)
;; (setq ivy-count-format "(%d/%d) ")
;; (defun my-counsel-ag ()
;;   (interactive)
;;   (counsel-ag nil default-directory))
;; (global-set-key (kbd "C-x C-y") 'my-counsel-ag)
;; (global-set-key (kbd "C-x B") 'counsel-recentf)
;; (global-set-key (kbd "C-x Y") 'counsel-yank-pop)
;; (global-set-key (kbd "C-M-j") 'avy-goto-char-timer)
;; (global-set-key (kbd "M-j") 'avy-goto-line)
;; (defun ivy-icomplete (f &rest r)
;;   (icomplete-mode -1)
;;   (unwind-protect
;;        (apply f r)
;;     (icomplete-mode 1)))
;; (advice-add 'ivy-read :around #'ivy-icomplete)
(global-set-key (kbd "M-k") 'avy-goto-char-timer)
(global-set-key (kbd "M-j") 'avy-goto-line)

;; 3 - TabNine: AI based completion
;; this requires clang to be installed:
;; sudo apt-get install clang-7 lldb-7 lld-7 --fix-missing
;; sudo ln -s /usr/bin/clang-7 /usr/bin/clang
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
;; (setq company-idle-delay 0) ;; Trigger completion immediately.
;; (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
;; (add-hook 'gud-gdb-mode-hook (lambda() (company-mode 0))) ;; Do not use company-mode in gud-gdb mode
;; ;; (add-hook 'wl-summary-mode-hook (lambda() (company-mode 0))) ;; too slow to have this on
;; (global-set-key (kbd "C-c z") 'company-mode) ;; toggle company-mode

(global-unset-key (kbd "C-z"))
(use-package company-tabnine
  :defer 1
  :custom
  (company-tabnine-max-num-results 9)
  :bind
  (("M-q" . company-other-backend)
   ("C-z" . company-tabnine))
  :init
  (defun company-tabnine-toggle (&optional enable)
    "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
    (interactive)
    (if (or enable (not (memq 'company-tabnine company-backends)))
        (progn
          (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
          (add-to-list 'company-backends #'company-tabnine)
          (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
          (message "TabNine enabled."))
      (setq company-backends (delete 'company-tabnine company-backends))
      (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
      (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
      (company-tabnine-kill-process)
      (message "TabNine disabled.")))
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  (defun lsp-after-open-tabnine ()
    "Hook to attach to `lsp-after-open'."
    (setq-local company-tabnine-max-num-results 3)
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  (company-tabnine-toggle t))

;; 
;; Other utils
;;

;; search string in all opened buffers (C-x g). I know that string is in my Emacs somewhere! 
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

;; Anzu
(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))
(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
(global-set-key (kbd "C-M-d") 'anzu-query-replace-at-cursor-thing)

;; Origami folding
(require 'origami)
(global-origami-mode t)

(global-set-key (kbd "C-c f f") 'origami-close-node)
(global-set-key (kbd "C-c f o") 'origami-open-node)

(global-set-key (kbd "C-c f g") 'origami-close-node-recursively)
(global-set-key (kbd "C-c f p") 'origami-open-node-recursively)

(global-set-key (kbd "C-c f A") 'origami-open-all-nodes)
(global-set-key (kbd "C-c f a") 'origami-close-all-nodes)

(global-highlight-parentheses-mode)
(toggle-scroll-bar -1)
(global-linum-mode -1)

;; Ruby
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

;; Racket
(setq racket-program "/Applications/Racket v8.0/bin/racket")

;; LSP
(use-package lsp-ui)
(require 'lsp-mode)
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'enh-ruby-mode-hook #'lsp)
  (add-hook 'ocaml-mode-hook #'lsp)
  (add-hook 'tuareg-mode-hook #'lsp)
  (add-hook 'racket-mode-hook #'lsp)
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  (setq lsp-signature-auto-activate nil)
  )

(require 'lsp-racket)

(require 'racket-mode)

(defun lsp-clients-ruby-make-options ()
  `(:solargraph.diagnostics t))

;; Ruby
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                  :major-modes '(ruby-mode enh-ruby-mode)
                  :priority -1
                  :multi-root t
                  :initialization-options #'lsp-clients-ruby-make-options
                  :server-id 'ruby-ls))

(define-key lsp-mode-map (kbd "C-x i") 'lsp-rename)
(define-key lsp-mode-map (kbd "C-c t .") 'lsp-ui-peek-find-definitions)
(define-key lsp-mode-map (kbd "C-c t /") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map (kbd "C-c m q") 'lsp-format-region)

(global-set-key (kbd "C-x C-a C-b") 'realgud:cmd-break)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'realgud-byebug)
(require 'realgud-lldb)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; focus mode
'((prog-mode . defun) (text-mode . sentence))

(setq markdown-fontify-code-blocks-natively t)

(electric-pair-mode 1)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(load-file "~/.emacs.rc/local/dot-mode.el")
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)
(global-set-key (kbd "M-.") 'dot-mode-execute)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "C-x c") 'seeing-is-believing-clear)
(global-set-key (kbd "C-x .") 'seeing-is-believing-run-as-xmpfilter)

(require 'ruby-mode)
(setq package-list '(better-defaults
                     solarized-theme
                     helm-projectile
                     helm-ag
                     ruby-electric
                     seeing-is-believing
                     chruby
                     inf-ruby))
;; ...
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(setenv "PATH" (concat "/usr/local/smlnj/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/smlnj/bin"  exec-path))

(xterm-mouse-mode 1)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e)) 
(setq mouse-sel-mode t)

(xclip-mode 1)

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(require 'smartparens-config)
(use-package smartparens
  :demand
  :config
  (smartparens-mode t)
  :bind
  ("C-M-n" . sp-forward-sexp)
  ("C-M-p" . sp-backward-sexp)
  )
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'ruby-mode-hook #'smartparens-mode)
(add-hook 'sml-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook (lambda () (smartparens-mode -1)) t)

(require 'yasnippet)
(yas-global-mode 1)

(require 'workgroups)
(setq wg-prefix-key (kbd "C-c z"))
(workgroups-mode 1)

;; fix the tab size
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; (defun turn-off-eldoc () (eldoc-mode -1))
;; (add-hook 'eval-expression-minibuffer-setup-hook #'turn-off-eldoc)
;; (add-hook 'racket-mode-hook #'turn-off-eldoc)

;; Go Lang
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'gorepl-mode)

;; Leet Code
(setq leetcode-prefer-language "python3")
(setq leetcode-prefer-sql "mysql")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/repos/code/leet")
    
;; Roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "/Users/jiaxingzhang/Google Drive/www/roam"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


