# Jiaxing Zhang's Emacs configuration

## Quick setup

1. Most packages can be installed through melpa. A few can be done through Github + Googling. A few core packages I've used:
a. helm
b. company-mode and tabnine
c. ace-window
...

2. You will need a master emacs file, like .emacs or alike, and put the following inside:

```elisp
;; (todo): if you have a global path to set

;; Load other el files
(load-file "~/.emacs.rc/general.el")
(load-file "~/.emacs.rc/layout.el")
(load-file "~/.emacs.rc/prog.el")
(load-file "~/.emacs.rc/life.el")
(load-file "~/.emacs.rc/org.el")
(load-file "~/.emacs.rc/deprecated.el")
```
