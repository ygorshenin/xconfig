(transient-mark-mode 1)
(global-hl-line-mode 't)

(require 'color-theme)
(setq color-theme-s-global t)
(require 'color-theme-solarized)
(setq solarized-use-terminal-theme t)

(load-file "~/.emacs.d/google-c-style.el")
(load-file "~/.emacs.d/llvm-mode.el")

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow)))
(add-hook 'c++-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow)))
