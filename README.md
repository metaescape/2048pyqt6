# QT6 2048

h/j/k/l arrow

digit + r: restart

## Required packages
python
python-pyqt6

# Usage

## Python
```python
python 2048/buffer.py 4  # start a 4x4 grid game
```

## EAF

[Install EAF](https://github.com/emacs-eaf/emacs-application-framework#install) first, then add below code in your emacs config:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-2048)
```

M-x eaf-open-2048

## original
https://github.com/jingdao/2048-pyqt 
