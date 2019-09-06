# Jiaxing Zhang's Emacs configuration

## Quick setup

1. Most packages can be installed through melpa. A few can be done through Github + Googling. A few core packages I've used:
a. helm
b. company-mode and tabnine
c. ace-window
...

2. You will need a master emacs file, like .emacs or alike, and put the following inside:

```elisp
;; (todo): Add paths to your packages here

;; Load other el files
(load-file "/home/jzhang/.emacs.rc/general.el")
(load-file "/home/jzhang/.emacs.rc/layout.el")
(load-file "/home/jzhang/.emacs.rc/tmw.el")
(load-file "/home/jzhang/.emacs.rc/deprecated.el")
```
