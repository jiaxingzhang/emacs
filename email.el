;; Email stuff

(require 'use-package)
(use-package wl
	     ;; The name of the package in MELPA is "wanderlust" but the
	     ;; feature provided by that package is 'wl
	     :ensure wanderlust
	     :commands (wl)
	     :bind
	     (:map wl-summary-mode-map
		   ; ("b a" . (lambda () (interactive) (djcb-wl-summary-refile "%Archives.2019")))
		   ;;Swap a and A in summary mode, so citing original message is on a and no-cite on A.
		   ("A" . wl-summary-reply)
		   ("a" . wl-summary-reply-with-citation)
		   ("r b" . jjgr-bbdb-mua-auto-update)
		   )
	     :hook
	     ((wl-mail-send-pre . djcb-wl-draft-subject-check)
	      (wl-mail-send-pre . djcb-wl-draft-attachment-check)
	      (wl-mail-setup . wl-draft-config-exec))

	     :config
	     (progn
	       (print "Wanderlust configured")
	       (defun jjgr-bbdb-mua-auto-update ()
		 (interactive)
		 (wl-summary-enter-handler)
		 (bbdb-mua-auto-update nil 'query)
		 (mime-preview-quit))

	       (defun djcb-wl-summary-refile (&optional folder)
		 "refile the current message to FOLDER; if FOLDER is nil, use the default"
		 (interactive)
		 (wl-summary-refile (wl-summary-message-number) folder)
		 (wl-summary-exec))

	       ;; Store sent emails in the current folder
	       (defun jjgr-determine-fcc-buffer ()
		 (if (or (equal wl-message-buffer-cur-folder "%INBOX")
			 (null wl-message-buffer-cur-folder))
		     "%Sent"
		   wl-message-buffer-cur-folder))
	       (setq wl-fcc 'jjgr-determine-fcc-buffer)

	       ;; Check messages for missing subject or abstract
	       (defun djcb-wl-draft-subject-check ()
		 "check whether the message has a subject before sending"
		 (if (and (< (length (std11-field-body "Subject")) 1)
			  (null (y-or-n-p "No subject! Send current draft?")))
		     (error "Abort.")))

	       (defun djcb-wl-draft-attachment-check ()
		 "if attachment is mention but none included, warn the the user"
		 (save-excursion
		   (goto-char 0)
		   (unless ;; don't we have an attachment?

		       (re-search-forward "^Content-Disposition: attachment" nil t)
		     (when ;; no attachment; did we mention an attachment?
			 (or (re-search-forward "attach" nil t)
			     (re-search-forward "adjunt" nil t))
		       (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
			 (error "Abort."))))))

	       (if (boundp 'mail-user-agent)
		   (setq mail-user-agent 'wl-user-agent))
	       (if (fboundp 'define-mail-user-agent)
		   (define-mail-user-agent
		     'wl-user-agent
		     'wl-user-agent-compose
		     'wl-draft-send
		     'wl-draft-kill
		     'mail-send-hook))

	       (require 'bbdb)
	       ) ; progn

	     :init
	     (setq wl-folders-file "~/.emacs.rc/dot.folders"

		   ;; SMTP server for mail posting.
		   wl-smtp-posting-server "smtp.gmail.com"
		   wl-smtp-posting-port 587
		   wl-smtp-posting-user "jiaxingzhang"
		   wl-smtp-authenticate-type "plain"
		   wl-smtp-connection-type 'starttls
		   wl-from "jiaxingzhang@gmail.com"
		   wl-local-domain "gmail.com"
		   
		   ;; Do not cache passwords. The cache corrupts server
		   ;; secrets.
		   password-cache nil
		   
		   elmo-imap4-default-user "jiaxingzhang@gmail.com"
		   elmo-imap4-default-server "imap.gmail.com"
		   elmo-imap4-default-port 993
		   elmo-imap4-default-authenticate-type 'clear
		   elmo-imap4-default-stream-type 'ssl
		   elmo-passwd-storage-type 'auth-source
		   elmo-imap4-use-modified-utf7 t
		   
		   ;; Location of archives
		   elmo-archive-folder-path "~/.Mail/"

		   ;; Location of MH and Maildir folders
		   elmo-localdir-folder-path "~/.Mail/"
		   elmo-maildir-folder-path "~/.Mail/"

		   wl-message-id-domain "jiaxingzhang@gmail.com"
		   wl-from "Jiaxing Zhang <jiaxingzhang@gmail.com>"
;		   mime-edit-default-signature "~/.emacs.rc/dot.signature"
		   wl-forward-subject-prefix "Fwd: "

		   wl-default-folder "%INBOX" ;; my main inbox
		   wl-biff-check-folder-list '("%INBOX") ;; check every 180 seconds
		   wl-biff-check-interval 180

		   wl-draft-folder "%Drafts"  ;; store drafts in 'postponed'
		   wl-trash-folder "%Trash"   ;; put trash in 'trash'

		   wl-stay-folder-window t
		   wl-folder-window-width 25
		   wl-folder-use-frame nil

		   wl-message-ignored-field-list '("^.*")
		   wl-message-visible-field-list '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:")
		   wl-message-sort-field-list wl-message-visible-field-list
		   wl-summary-width 120 ;; No width
		   wl-summary-default-sort-spec 'date
		   wl-message-window-size '(1 . 2)

		   ;; Always download emails without confirmation
		   wl-prefetch-threshold nil
		   wl-message-buffer-prefetch-threshold nil
		   elmo-message-fetch-threshold nil

		   ;; Rendering of messages using 'shr', Emacs' simple html
		   ;; renderer, but without fancy coloring that distorts the
		   ;; looks
		   mime-view-text/html-previewer 'shr
		   shr-use-colors nil

;; wl-draft-config-alist
;; '(((string-match "1" "1")
;;    (bottom . "\n--\n") (bottom-file . "~/OnlineFolder/Library/dot.signature"))
;;   )

		   ;; don't ****ing split large messages
		   mime-edit-split-message nil
		   )
	     ) ; use-package wanderlust

(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)

(use-package bbdb
	     :ensure t
	     :commands (bbdb-initialize)
	     :hook
	     ((mail-setup . bbdb-mail-aliases)
	      (message-setup . bbdb-mail-aliases)
	      (wl-mail-setup . jjgr-add-bbdb-tab-completion))

	     :init
	     (setq bbdb-file "~/.emacs.rc/bbdb"
		   bbdb-mua-pop-up t
		   bbdb-mua-pop-up-window-size t)

	     :config
	     (progn
	       (bbdb-initialize 'wl)
	       (bbdb-mua-auto-update-init 'wl)

	       (defun my-bbdb-complete-mail ()
		 "If on a header field, calls `bbdb-complete-mail' to complete the name."
		 (interactive)
		 (when (< (point)
			  (save-excursion
			    (goto-char (point-min))
			    (search-forward (concat "\n" mail-header-separator "\n") nil 0)
			    (point)))
		   (bbdb-complete-mail)))

	       (defun jjgr-add-bbdb-tab-completion ()
		 (define-key (current-local-map) (kbd "<tab>")
		   'my-bbdb-complete-mail))
	       ) ; progn
	     ) ; use-package bbdb

(use-package bbdb-csv-import
	     :ensure t
	     :defer t
	     :commands (bbdb-csv-import-file bbdb-csv-import-buffer)
	     :after (bbdb)
	     :config
	     ;; Remove fields that Google Contacts creates, inserting them at the right
	     ;; position.
	     ;; 1. additional-name -> prepend it to lastname
	     (defun jjgr-fix-bbdb-csv-import ()
	       (interactive)
	       (dolist (r (bbdb-records))
		 (print r)
		 (let* ((fields (bbdb-record-xfields r))
			(aname (assoc 'additional-name fields)))
		   (when aname
		     (print aname)
		     (let ((new-lastname (concatenate 'string (cdr aname) " " (bbdb-record-lastname r))))
		       ;;(setf (bbdb-record-lastname r) new-lastname)
		       (bbdb-record-set-lastname r new-lastname)
		       (bbdb-record-set-xfields r (assoc-delete-all 'additional-name fields))
		       )))))
	     )


(defun my-wl-summary-prepared-hook () (save-excursion (wl-summary-rescan "number" t)))
(add-hook 'wl-summary-prepared-hook 'my-wl-summary-prepared-hook)

(defun wl-setup-headers ()
  (wl-draft-config-exec)
  (flyspell-mode t)
  (auto-fill-mode t))

 (add-hook 'wl-draft-mode-hook 'flyspell-mode)

;; from a WL-mailinglist post by David Bremner

;; Invert behaviour of with and without argument replies.
;; just the author
(setq wl-draft-reply-without-argument-list
  '(("Reply-To" ("Reply-To") nil nil)
     ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
     ("From" ("From") nil nil)))


;; bombard the world
(setq wl-draft-reply-with-argument-list
  '(("Followup-To" nil nil ("Followup-To"))
     ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
     ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
     ("From" ("From") ("To" "Cc") ("Newsgroups"))))

(require 'wl-spam)
(wl-spam-setup)
(setq elmo-spam-scheme 'sa)   ;; sa for spamassassin, see the elmo-spam-scheme
                              ;; docs for alternatives
(setq wl-spam-folder ".spam") ;; maildir to store spam

;; List of identities
;; choose template with C-c C-j
(setq wl-template-alist
      '(("personal"
         (wl-from . "Jiaxing Zhang <jiaxingzhang@gmail.com>")
         ("From" . wl-from))
        ("business"
         (wl-from . "Jiaxing Zhang <jzhang@mathworks.com>")
         ("From" . wl-from)
         ("Organization" . "The MathWorks Inc."))
        ))
