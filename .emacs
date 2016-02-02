(defun is-osx () (string= "darwin" system-type))

(defvar *packages* '(magit color-theme color-theme-solarized color-theme-sanityinc-solarized
                           clang-format google-c-style
                           haskell-mode go-mode slime helm helm-projectile))
(defvar *font-family* (if (is-osx)
                          "Courier New"
                        "DejaVu Sans Mono"))
(defvar *font-height* (if (is-osx)
                          240
                        120))

(when (is-osx)
  (setq mac-allow-anti-aliasing nil))

(defun init-packages ()
  (package-initialize)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))

(defun download-packages ()
  "Downloads all necessary packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package *packages*)
    (unless (package-installed-p package)
      (message "Installing %s" package)
      (package-install package))))

(defun switch-to-shell ()
  "Switches to shell buffer"
  (interactive)
  (let* ((buffer-names (mapcar #'buffer-name (buffer-list)))
         (shell-name "*shell*")
         (shell-window (get-buffer-window shell-name)))
    (cond (shell-window
           (select-window shell-window))
          ((find-if #'(lambda (name) (string= shell-name name)) buffer-names)
           (pop-to-buffer shell-name))
          (t (shell)))))

(defun init-common-edit-mode ()
  (transient-mark-mode 1)
  (global-hl-line-mode 't)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab
        compilation-scroll-output 'first-error
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
  (put 'upcase-region 'disabled nil)
  (global-set-key (kbd "C-c C-l") 'sort-lines)
  (global-set-key (kbd "C-c l") 'sort-lines)
  (global-set-key (kbd "C-c C-p") 'switch-to-shell)
  (global-set-key (kbd "C-c p") 'switch-to-shell))

(defun customize-common-coding-mode-map (mode-map)
  (define-key mode-map (kbd "C-c C-c") 'recompile)
  (define-key mode-map (kbd "C-c C-p") 'switch-to-shell)
  (define-key mode-map (kbd "C-c p") 'switch-to-shell)
  (define-key mode-map (kbd "C-c C-r") 'clang-format-region)
  (define-key mode-map (kbd "C-c C-f") 'clang-format-buffer))

(defun init-common-coding-mode ()
  (require 'clang-format)
  (require 'google-c-style)

  (if (is-osx)
      (setq clang-format-executable "/usr/local/bin/clang-format")
    (setq clang-format-executable "~/coding/llvm-3.6.1/build/bin/clang-format"))

  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (add-hook 'c-mode-hook (lambda () (customize-common-coding-mode-map c-mode-map)))
  (add-hook 'c++-mode-hook (lambda () (customize-common-coding-mode-map c++-mode-map)))
  (add-hook 'java-mode-hook (lambda () (customize-common-coding-mode-map java-mode-map))))

(defun init-clisp-mode ()
  (require 'slime)
  (setq inferior-lisp-program "/usr/bin/sbcl"
        common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  (global-set-key [(f2)] 'slime-hyperspec-lookup))

(defun init-haskell-mode ()
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-indent)
                                 (define-key haskell-mode-map (kbd "C-c C-c") 'recompile))))

(defun init-go-mode ()
  (require 'go-mode)
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save))

(defun init-helm-mode ()
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

  (setq helm-display-header-line nil
        helm-split-window-in-side-p t)
  (helm-mode 1)

  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-indexing-method 'alien)
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

(defun toggle-dark-light ()
  "Switches between dark and light solarized themes."
  (interactive)
  (cond ((find 'solarized custom-enabled-themes)
         (disable-theme 'solarized)
         (ecase frame-background-mode
           ((nil light) (customize-set-variable 'frame-background-mode 'dark))
           ((dark) (customize-set-variable 'frame-background-mode 'light)))
         (load-theme 'solarized))
        ((find 'sanityinc-solarized-dark custom-enabled-themes)
         (disable-theme 'sanityinc-solarized-dark)
         (load-theme 'sanityinc-solarized-light t))
        ((find 'sanityinc-solarized-light custom-enabled-themes)
         (disable-theme 'sanityinc-solarized-light)
         (load-theme 'sanityinc-solarized-dark t))))

(defun init-color-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (require 'color-theme-sanityinc-solarized)
  (set-frame-parameter nil 'background-mode 'dark)
  (load-theme 'solarized)
  ;; (load-theme 'sanityinc-solarized-dark t)
  (global-set-key (kbd "<f11>") 'toggle-dark-light))

(defun init-snippets ()
  (require 'yasnippet)
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets")
	yas-prompt-functions
        '(yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))
  (yas-global-mode 1))

(defun init-fullscreen ()
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(setq inhibit-splash-screen t)
(init-packages)
(init-common-edit-mode)
(init-common-coding-mode)
(init-clisp-mode)
(init-haskell-mode)
(init-go-mode)
(init-rcirc)
(init-helm-mode)
(init-magit-mode)
(init-color-theme)
(init-snippets)
(init-fullscreen)

(shell)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "unknown" :slant normal :weight normal :height 240 :width normal))))
 '(gnus-header-from ((t (:inherit message-header-other-face :foreground "medium violet red" :weight normal))))
 '(gnus-header-subject ((t (:inherit message-header-subject :weight normal))))
 '(magit-diff-context-highlight ((t (:inherit nil))))
 '(magit-section-highlight ((t (:inherit highlight)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))))
