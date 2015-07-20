(transient-mark-mode 1)
(global-hl-line-mode 't)

(load-file "~/.emacs.d/google-c-style.el")
(load-file "~/.emacs.d/llvm-mode.el")
(load "~/.emacs.d/cmake-mode.el")

(load "~/.emacs.d/clang-format.el")

(global-set-key (kbd "C-c C-r") 'clang-format-region)
(global-set-key (kbd "C-c C-f") 'clang-format-buffer)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow)))
(add-hook 'c++-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow)))

(require 'rcirc)
(setq rcirc-default-nick "ygorshenin")
(setq rcirc-default-user-name "ygorshenin")
(setq rcirc-default-full-name "Yuri Gorshenin")
(rcirc-track-minor-mode 1)
(add-to-list 'rcirc-server-alist
	     '("irc.oftc.net"
	       :channels ("#llvm")))
(add-to-list 'rcirc-server-alist
	     '("irc.freenode.net"
	       :channels ("#chromium")))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(put 'upcase-region 'disabled nil)

(setq gofmt-command "goimports")
(load-file "~/.emacs.d/go-mode.el")
(add-hook 'before-save-hook 'gofmt-before-save)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
(global-set-key [(f2)] 'slime-hyperspec-lookup)
