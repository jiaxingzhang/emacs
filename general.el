;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is a general set-up that should apply to all sessions

;; Package archives
;; Out of China, this is so far the best mirrors
;; Within China, we should replace these with TsingHua's mirrors
;; Hack for using a different set of repositories when ELPA is down

;; always start as a server
;; (server-start) ;; no need to do this in MW environment

;; theme
;; TODO (2021-05-19) - i haven't found a theme I really like yet
;; (cond ((eq system-type 'darwin)
;;        (load-theme 'dracula t)
;;        )
;;       ((eq system-type 'gnu/linux)
;;        (load-theme 'monokai t)
;;        ))

;; FIXME (2021-05-19) - remove this?
;; icons
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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
(defalias 'o 'find-file)
(defalias 'oo 'find-file-other-window)
(setq eshell-scroll-to-bottom-on-input t)

;; General Settings
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
(global-linum-mode 1)
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
(blink-cursor-mode t)
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
;; (setq kill-ring-max 200)
(setq apropos-do-all t)
(setq-default ispell-program-name "aspell")
(savehist-mode 1)
(put 'narrow-to-region 'disabled nil)
;; set the clipboard
(setq select-enable-clipboard t)
(if (eq system-type 'gnu-linux) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])

;; Coding 
(defun cpplint ()
 "call CPPLINT in curent buffer"
 (interactive)
 (flycheck-compile 'c/c++-googlelint) (other-window -1))

; mark ring size
(setq mark-ring-max 99)
(setq global-mark-ring-max 99)

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
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; (helm-mode 0)
;; (global-set-key (kbd "C-c x") 'helm-mode)

;; ivy-mode as a replacement for helm
;; I still like ivy in general 
(ivy-mode 1)

;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-height 20)
;; (setq ivy-count-format "(%d/%d) ")
;; ;; (setq ivy-extra-directories ())
(setq ivy-extra-directories  '("./"))
(defun my-counsel-ag ()
  (interactive)
  (counsel-ag nil default-directory))

(defun ivy-icomplete (f &rest r)
  (icomplete-mode -1)
  (unwind-protect
       (apply f r)
    (icomplete-mode 1)))
(advice-add 'ivy-read :around #'ivy-icomplete)

;; 3 - TabNine: AI based completion
;; this requires clang to be installed:
;; sudo apt-get install clang-7 lldb-7 lld-7 --fix-missing
;; sudo ln -s /usr/bin/clang-7 /usr/bin/clang
(require 'company-tabnine)
(setq company-idle-delay 0) ;; Trigger completion immediately.
(setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
(add-hook 'gud-gdb-mode-hook (lambda() (company-mode 0))) ;; Do not use company-mode in gud-gdb mode
(add-hook 'wl-summary-mode-hook (lambda() (company-mode 0))) ;; too slow to have this on

(defun toggle-tabnine ()
  (interactive)
  (if (get 'toggle-tabnine-state 'state)
      (progn
        (add-to-list 'company-backends #'company-tabnine)
        (put 'toggle-tabnine-state 'state nil)
        (message "Tabnine is on")
        )
    (progn
      (setq company-backends (delete 'company-tabnine company-backends))
      (put 'toggle-tabnine-state 'state t)
      (message "Tabnine is off")
      )))

;; 
;; Other utils
;;

;; search string in all opened buffers (C-x g). I know that string is in my Emacs somewhere! 
;; FIXME (2021-05-19) - remove this?
;; (require 'cl)
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

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max))
        )
      (message filename))))

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

;; FIXME (2021-05-19) - remove this?
;; ;; Origami folding
;; (require 'origami)
;; (global-origami-mode t)

;; (global-set-key (kbd "C-c f f") 'origami-close-node)
;; (global-set-key (kbd "C-c f o") 'origami-open-node)
;; (global-set-key (kbd "C-c f g") 'origami-close-node-recursively)
;; (global-set-key (kbd "C-c f p") 'origami-open-node-recursively)
;; (global-set-key (kbd "C-c f A") 'origami-open-all-nodes)
;; (global-set-key (kbd "C-c f a") 'origami-close-all-nodes)
;; (load-file "~/.emacs.rc/modeline.el")



;; FIXME (2021-05-19) - remove this?
;; (load-file "/home/jzhang/.emacs.rc/local/ace-jump-mode.el")
;; ;;
;; ;; ace jump mode major function
;; ;; 
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; ;; you can select the key you prefer to
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; 
;; enable a more powerful jump back function from ace jump mode
;;

;; FIXME (2021-05-19) - remove this?
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; FIXME (2021-05-19) - remove this?
;; (load-file "/home/jzhang/.emacs.rc/local/find-file-in-project.el")
;; (require 'find-file-in-project)

;; FIXME (2021-05-19) - comment?
(electric-pair-mode 1)

;; so that in mouse works in termial
(xclip-mode 1)
(xterm-mouse-mode 1)

;; smart model line
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; FIXME (2021-05-19) - comment?
(require 'expand-region)

;; FIXME (2021-05-19) - remove this?
;; (load-file "/home/jzhang/.emacs.rc/local/auto-mark.el")
;; (when (require 'auto-mark nil t)
;;   (setq auto-mark-command-class-alist
;;         '((anything . anything)
;;           (goto-line . jump)
;;           (indent-for-tab-command . ignore)
;;           (undo . ignore)))
;;   (setq auto-mark-command-classifiers
;;         (list (lambda (command)
;;                 (if (and (eq command 'self-insert-command)
;;                          (eq last-command-char ? ))
;;                     'ignore))))
;;   (global-auto-mark-mode 1))

(load-file "~/.emacs.rc/local/dot-mode.el")
(require 'dot-mode)
;; FIXME (2021-05-19) - remove this?
;; (add-hook 'find-file-hooks 'dot-mode-on)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun er-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; store all backup and autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-save/" t)))

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; (setq create-lockfiles nil)

(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)


;; FIXME (2021-05-19) - clean up the following into use-package
(flycheck-mode 1)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))
(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode))
(custom-set-variables
   '(flycheck-googlelint-verbose "3")
   '(flycheck-googlelint-filter "-whitespace,+whitespace/braces"))

;; FIXME (2021-05-19) - comment?
(global-highlight-parentheses-mode)

;; restore the layouts after ediff
(when (fboundp 'winner-mode) (winner-mode 1))
(defvar my-ediff-last-windows nil)
(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))
(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(require 'iflipb)
;; (defun my-iflipb-buffer-list ()
;;   "Returns list of buffers whose major-mode is the same as current buffer's one."
;;   (let ((cur-buf-list (buffer-list (selected-frame)))
;;         (same-major-mode-buflist nil)
;;         (currbuf-major-mode
;;          (buffer-local-value 'major-mode (current-buffer))))
;;      (dolist (buffer cur-buf-list)
;;       (if (eq (buffer-local-value 'major-mode buffer) currbuf-major-mode)
;;           (add-to-list 'same-major-mode-buflist buffer)))
;;      (nreverse same-major-mode-buflist)))
;;(setq iflipb-buffer-list-function 'my-iflipb-buffer-list)
(setq iflipb-wrap-around t)

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; cycle windows backwards
(defun prev-window ()
   (interactive)
   (other-window -1))

(defun switch-to-previous-buffer-in-a-different-window ()
  (interactive)
  (let* ((otherbuf (other-buffer (current-buffer) t))
     (otherwin (get-buffer-window otherbuf)))
(if otherwin
    (select-window otherwin)
  (message "Last buffer (%s) is not currently visible" (buffer-name otherbuf)))))

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;; set evil default state to emacs
;; (setq evil-default-state 'emacs)

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;;
;; Key Bindings
;; 
(global-set-key (kbd "C-c q") 'remember-notes)
(global-set-key (kbd "C-c Q") 'remember-other-frame)

(global-set-key (kbd "C-c l") 'global-hl-line-mode) ;; toggle highlight the current line
(global-set-key (kbd "C-c b") 'global-hl-block-mode) ;; toggle highlight the current block

(global-set-key (kbd "C-c m l") 'cpplint)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-r") 'helm-do-grep-ag)
;; (global-set-key (kbd "C-x f") 'helm-ag-this-file)
;; (global-set-key (kbd "M-x") #'helm-M-x) ; a bit too much I think

(global-set-key (kbd "C-x C-y") 'my-counsel-ag)
(global-set-key (kbd "C-x B") 'counsel-recentf)
(global-set-key (kbd "M-l") 'counsel-mark-ring)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "M-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x Y") 'counsel-yank-pop)

(global-set-key (kbd "M-k") 'avy-goto-char-timer)
(global-set-key (kbd "M-j") 'avy-goto-line)

;; (global-set-key (kbd "C-c a") 'ivy-mode)

(global-set-key (kbd "C-c z") 'toggle-tabnine)

(global-set-key (kbd "C-x g") 'search-all-buffers)

(global-set-key (kbd "C-x y") 'my-put-file-name-on-clipboard)

(global-set-key (kbd "C-x i") 'anzu-query-replace-at-cursor-thing)

(global-set-key (kbd "C-=") 'er/expand-region)

;; quite nice to mimic dot in vim
(global-set-key (kbd "C-.") 'dot-mode-execute)

;; FIXME (2021-05-19) Is this really needed?
(global-set-key (kbd "C-c t i") 'counsel-imenu)

(global-set-key (kbd "C-x C-a C-h") 'winner-undo)
(global-set-key (kbd "C-x C-a C-l") 'winner-redo)

(global-set-key (kbd "M-o") 'iflipb-next-buffer)
(global-set-key (kbd "M-i") 'iflipb-previous-buffer)

;; (global-set-key (kbd "C-z") 'sb-toggle-evil-mode)

(global-set-key (kbd "C-x |") 'window-toggle-split-direction)

(define-key global-map (kbd "C-x p") 'prev-window)

(global-set-key [?\C-x ?l] 'switch-to-previous-buffer-in-a-different-window)

;;
;; Use-package
;;
;; I like centaur tab over my own
(require 'use-package)
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("M-{" . centaur-tabs-backward)
  ("M-}" . centaur-tabs-forward))

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  ;; FIXME (2021-05-19) - comment?
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/local/bin/fish")
  ;; (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))  
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (setq shell-pop-autocd-to-working-dir nil) ; don't auto-cd 
  )

;; LSP
(require 'lsp-ui)
(require 'lsp-mode)

;; sbtools turn this on by default?
(cond ((eq system-type 'darwin)
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
;;	 (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
	 (setq lsp-signature-auto-activate nil)
	 )
       (define-key lsp-mode-map (kbd "C-x i") 'lsp-rename)
       (define-key lsp-mode-map (kbd "C-c t .") 'lsp-ui-peek-find-definitions)
       (define-key lsp-mode-map (kbd "C-c t /") 'lsp-ui-peek-find-references)
       (define-key lsp-mode-map (kbd "C-c m q") 'lsp-format-region)
       ;; Leet Code
       (setq leetcode-prefer-language "cpp")
       (setq leetcode-prefer-sql "mysql")
       (setq leetcode-save-solutions t)
       (setq leetcode-directory "~/repos/code/leet")
       )
      ((eq system-type 'gnu/linux)
       (global-undo-tree-mode 0)
       ))

(defun write-region-to-client-clipboard (beg end)
  (interactive "r")
  (copy-region-as-kill beg end)
  (shell-command-on-region beg end "ssh jzhang@ah-jzhang-maci pbcopy" nil nil nil t)
  (sb-copy-to-clipboard)
  )

(cond ((eq system-name 'ah-jzhang-maci)
       (global-set-key (kbd "C-x C-q") 'write-region-to-client-clipboard-search)
       (global-set-key (kbd "M-w") 'write-region-to-client-clipboard)
       )
      ((eq system-type 'gnu/linux)
       ))


(defun write-region-to-client-clipboard-search (beg end)
  (interactive "r")
  (copy-region-as-kill beg end)
  (shell-command-on-region beg end "ssh jzhang@ah-jzhang-maci codesearch" nil nil nil t))

;; strange behavior on iTerm
(if (not (display-graphic-p))
    (progn
      (global-set-key (kbd "M-SPC") 'mark-sexp)
      (set-face-attribute 'linum nil :background "color-251")
      )
  ) 

;; (defun cut-wrapper (begin end)
;;   (interactive "r")
;;   (progn   (kill-region begin end)
;;            (sb-cut-to-clipboard))

;;   )
;; (global-set-key (kbd "C-w") 'cut-wrapper)

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'powerline)

;; These two lines are just examples
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
;; These two lines you really need.
(setq sml/theme 'light-powerline)
(sml/setup)

(yas-global-mode 1)
