(transient-mark-mode 1)
(global-hl-line-mode 't)

(load-file "~/.emacs.d/google-c-style.el")
(load-file "~/.emacs.d/llvm-mode.el")

(load "~/.emacs.d/clang-format.el")
(global-set-key (kbd "C-M-f") 'clang-format-region)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow)))
(add-hook 'c++-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow)))

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
