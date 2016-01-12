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
  (put 'upcase-region 'disabled nil)
  (setq compilation-scroll-output 'first-error))

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
  (setq inferior-lisp-program "/usr/bin/sbcl"
        common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  (global-set-key [(f2)] 'slime-hyperspec-lookup))

(defun init-haskell-mode ()
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(defun init-go-mode ()
  (require 'go-mode)
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save))

(defun init-helm ()
  (require 'helm)
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

  (global-unset-key (kbd "C-x c"))

  (setq helm-split-window-in-side-p t)
  (helm-mode 1)

  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(defun init-magit-mode ()
  (require 'magit)
  (global-set-key (kbd "C-c C-g") 'magit-status))

(defun init-rcirc ()
  (require 'rcirc)
  (setq rcirc-default-nick "ygorshenin"
        rcirc-default-user-name "ygorshenin"
        rcirc-default-full-name "Yuri Gorshenin")
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

(defun init-snippets ()
  (require 'yasnippet)
  (yas-global-mode 1)
  (setq yas-snippet-dirs (cons "~/.emacs.d/snippets" yas-snippet-dirs)
        yas-prompt-functions
        '(yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt)))

(defun disable-bars ()
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(init-packages)
(init-common-edit-mode)
(init-c++-mode)
(init-clisp-mode)
(init-haskell-mode)
(init-go-mode)
(init-rcirc)
(init-helm)
(init-magit-mode)
(init-color-theme)
(init-snippets)
(disable-bars)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(magit-item-highlight ((t nil))))
