![CI](https://github.com/Sbozzolo/vterm-extra/workflows/CI/badge.svg)

# vterm-extra

Improve the user experience in [vterm](https://github.com/akermu/emacs-libvterm) with convenience functions.

## Installation

The most simple way to install this package is with `use-package`:
```emacs-lisp
(use-package vterm-extra
              :load-path  "/path/of/the/repo/"
              :bind ("s-t" . vterm-extra-dispatcher))
```

## Function available

### `vterm-extra-dispatcher`

`vterm-extra-dispatcher` allows  to easily switch  to VTerm buffers or  create a
new ones if. If there are no VTerm  buffers, or the current one is the only one,
`vterm-extra-dispatcher` creates  a new one. If  the current buffer is  the only
VTerm buffer, it creates  a new one. Otherwise, if there  are other VTerm buffer
there are other, it prompts the user for a new one to select (or to create), and
it   switches   to   that   buffer.    When   called   with   prefix   argument,
`vterm-extra-dispatcher` always creates a new VTerm.
