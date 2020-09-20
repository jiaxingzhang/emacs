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

(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook      #'enable-paredit-mode)

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (dolist (m '(emacs-lisp-mode-hook
;; 	       racket-mode-hook
;; 	       racket-repl-mode-hook))
;;     (add-hook m #'paredit-mode))
;;   (bind-keys :map paredit-mode-map
;; 	     ("{"   . paredit-open-curly)
;; 	     ("}"   . paredit-close-curly))
;;   (unless terminal-frame
;;     (bind-keys :map paredit-mode-map
;; 	       ("M-[" . paredit-wrap-square)
;; 	       ("M-{" . paredit-wrap-curly))))

;; basic keybindings for compile and debug
;; (global-set-key (kbd "C-c c") 'compile)
;; (global-set-key (kbd "C-c d") 'gud-gdb)

;; Stackoverflow in Emacs
;; (require 'json)
;; (require 'helm-net)
;; (defun my-get-stackoverflow-answers (query)
;;   (interactive "sQuestion: ")
;;   (let* ((question_ids
;;           (with-current-buffer
;;               (url-retrieve-synchronously
;;                (concat "https://google.com/search?ie=utf-8&oe=utf-8&hl=en&as_qdr=all&q="
;;                        (url-hexify-string (concat query " site:stackoverflow.com"))))
;;             (let (ids)
;;               (while (re-search-forward "https://stackoverflow.com/questions/\\([0-9]+\\)" nil t)
;;                 (push (match-string-no-properties 1) ids))
;;               (setq ids (reverse ids))
;;               (if (> (length ids) 5)
;;                   (subseq ids 0 5)
;;                 ids))))

;;          (url_template (format "https://api.stackexchange.com/2.2/questions/%s%%s?site=stackoverflow.com"
;;                                (string-join question_ids ";")))

;;          (questions (with-current-buffer                      
;;                         (url-retrieve-synchronously
;;                          (format url_template ""))
;;                       (goto-char (point-min))
;;                       (search-forward "\n\n")
;;                       (append (assoc-default 'items (json-read)) nil)))

;;          (answers (with-current-buffer
;;                       (url-retrieve-synchronously
;;                        (concat (format url_template "/answers")
;;                                "&order=desc&sort=activity&filter=withbody"))
;;                     (goto-char (point-min))
;;                     (search-forward "\n\n")
;;                     (sort (append (assoc-default 'items (json-read)) nil)
;;                           (lambda (x y)
;;                             (> (assoc-default 'score x)
;;                                (assoc-default 'score y)))))))

;;     (switch-to-buffer "*stackexchange*")
;;     (erase-buffer)

;;     (dolist (question_id (mapcar 'string-to-number question_ids))
;;       (let ((question (some (lambda (question)
;;                               (if (equal (assoc-default 'question_id question)
;;                                          question_id)
;;                                   question))
;;                             questions)))
;;         (insert "<hr><h2 style='background-color:paleturquoise'>Question: "
;;                 (format "<a href='%s'>%s</a>"
;;                         (assoc-default 'link question)
;;                         (assoc-default 'title question))
;;                 "</h2>"
;;                 "\n"
;;                 (mapconcat
;;                  'identity
;;                  (let ((rendered
;;                         (remove-if
;;                          'null
;;                          (mapcar (lambda (answer)
;;                                    (if (and (equal question_id
;;                                                    (assoc-default 'question_id answer))
;;                                             (>= (assoc-default 'score answer) 0))
;;                                        (concat "<hr><h2 style='background-color:"
;;                                                "#c1ffc1'>Answer - score: "
;;                                                (number-to-string (assoc-default 'score answer))
;;                                                "</h2>"
;;                                                (assoc-default 'body answer))))
;;                                  answers))))
;;                    (if (> (length rendered) 5)
;;                        (append (subseq rendered 0 5)
;;                                (list (format "<br><br><a href='%s'>%s</a>"
;;                                              (assoc-default 'link question)
;;                                              "More answers...")))
;;                      rendered))
;;                  "\n")
;;                 )))
;;     (shr-render-region (point-min) (point-max))
;;     (goto-char (point-min))
;;     (save-excursion
;;       (while (search-forward "^M" nil t)
;;         (replace-match "")))))
;; (defun my-helm-stackoverflow-lookup ()
;;   (interactive)
;;   ;; set debug-on-error to swallow potential network errors
;;   ;; idea taken from: https://blog.johnregner.com/post/78877988910/fixing-helm-spotify#_=_
;;   (let ((debug-on-error t)
;;         (helm-google-suggest-actions '(("Stackoverflow" . my-get-stackoverflow-answers))))
;;     (helm-google-suggest)))
;; (global-set-key (kbd "C-c f") 'my-helm-stackoverflow-lookup)


