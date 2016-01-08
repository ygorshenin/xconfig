(defvar *packages* '(magit color-theme clang-format google-c-style haskell-mode go-mode slime))

(defun init-packages ()
  (package-initialize)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))

(defun download-packages ()
  ; Downloads all necessary packages.
  (interactive)
  (dolist (package *packages*)
    (unless (package-installed-p package)
      (message "Installing %s" package)
      (package-install package))))

(defun init-common-edit-mode ()
  (transient-mark-mode 1)
  (global-hl-line-mode 't)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (put 'upcase-region 'disabled nil))

(defun init-c++-mode ()
  (require 'clang-format)
  (require 'google-c-style)

  (global-set-key (kbd "C-c C-r") 'clang-format-region)
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (add-hook 'c-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow)))
  (add-hook 'c++-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow))))

(defun init-clisp-mode ()
  (require 'slime)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  (global-set-key [(f2)] 'slime-hyperspec-lookup))

(defun init-haskell-mode ()
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(defun init-go-mode ()
  (require 'go-mode)
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save))

(defun init-magit-mode ()
  (require 'magit)
  (global-set-key (kbd "C-c C-g") 'magit-status))

(defun init-rcirc ()
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
                 :channels ("#chromium"))))

(defun init-color-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-initialize)
  (color-theme-solarized-light))

(init-packages)
(init-common-edit-mode)
(init-c++-mode)
(init-clisp-mode)
(init-haskell-mode)
(init-go-mode)
(init-rcirc)
(init-magit-mode)
(init-color-theme)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(magit-item-highlight ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
