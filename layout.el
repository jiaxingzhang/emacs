;; Jiaxing Zhang's GNU/Emacs file
;; Note: This is for layout management

;; rebind window switching to ace-window
;; note: we need ace-window for this to work
(global-set-key (kbd "C-x m") 'ace-window)

(require 'column-marker)
;; To undo, C-u C-x n
(global-set-key [?\C-x ?n] 'column-marker-1)
;; To turn off showing the marker, C-u C-x e
(global-set-key [?\C-x ?e] (lambda () (interactive) (column-marker-2 80)))
;; Kinda slow if we turn on the hook, so just use C-x e to show 80 column marker
;; (add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))

 (require 'iflipb)
(global-set-key (kbd "M-o") 'iflipb-next-buffer)
(global-set-key (kbd "M-i") 'iflipb-previous-buffer)

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
(global-set-key (kbd "C-x |") 'window-toggle-split-direction)

(require 'layout-restore)
(global-set-key (kbd "C-x C-l") 'layout-save-current)
(global-set-key (kbd "C-x C-r") 'layout-restore)

;; bookmark features
(use-package bm
  :bind (("C-c C-x C-l" . bm-toggle)
         ("C-c C-x C-n" . bm-next)
         ("C-c C-x C-p" . bm-previous)))


;; cycle windows backwards
(defun prev-window ()
   (interactive)
   (other-window -1))
(define-key global-map (kbd "C-x p") 'prev-window)

(defun switch-to-previous-buffer-in-a-different-window ()
  (interactive)
  (let* ((otherbuf (other-buffer (current-buffer) t))
     (otherwin (get-buffer-window otherbuf)))
(if otherwin
    (select-window otherwin)
  (message "Last buffer (%s) is not currently visible" (buffer-name otherbuf)))))
(global-set-key [?\C-x ?l] 'switch-to-previous-buffer-in-a-different-window)

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)

;; Toggle window dedication
;; Press C-x d in each window you want to "freeze"
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key [?\C-x ?d] 'toggle-window-dedicated)

