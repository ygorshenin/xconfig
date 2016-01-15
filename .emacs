(defun is-osx () (string= "darwin" system-type))

(defvar *packages* '(magit color-theme color-theme-solarized color-theme-sanityinc-solarized
                           clang-format google-c-style
                           haskell-mode go-mode slime helm helm-projectile))
(defvar *font-family* (if (is-osx)
                          "Monaco"
                          "DejaVu Sans Mono"))
(defvar *font-height* (if (is-osx)
                          160
                          120))

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
  (setq indent-line-function 'insert-tab)
  (put 'upcase-region 'disabled nil)
  (setq compilation-scroll-output 'first-error)
  (global-set-key (kbd "C-c C-l") 'sort-lines)
  (global-set-key (kbd "C-c l") 'sort-lines)
  (global-set-key (kbd "C-c C-p") 'switch-to-shell)
  (global-set-key (kbd "C-c p") 'switch-to-shell)
  (global-set-key (kbd "C-c C-c") 'recompile))

(defun init-c++-mode ()
  (require 'clang-format)
  (require 'google-c-style)

  (defun customize-keys (mode-map)
    (define-key mode-map (kbd "C-c C-c") 'recompile)
    (define-key mode-map (kbd "C-c C-p") 'switch-to-shell)
    (define-key mode-map (kbd "C-c p") 'switch-to-shell))

  (global-set-key (kbd "C-c C-r") 'clang-format-region)
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (add-hook 'c-mode-hook #'(lambda ()
                             (customize-keys c-mode-map)
                             (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow)))
  (add-hook 'c++-mode-hook #'(lambda ()
                               (customize-keys c++-mode-map)
                               (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow))))

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

(defun toggle-dark-light ()
  (interactive)
  (cond ((find 'sanityinc-solarized-dark custom-enabled-themes)
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
  (load-theme 'sanityinc-solarized-dark t)
  (global-set-key (kbd "<f11>") 'toggle-dark-light))

(defun init-snippets ()
  (require 'yasnippet)
  (yas-global-mode 1)
  (setq yas-snippet-dirs (cons "~/.emacs.d/snippets" yas-snippet-dirs)
        yas-prompt-functions
        '(yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt)))

(defun init-fullscreen ()
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(init-packages)
(init-common-edit-mode)
(init-c++-mode)
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
 `(default ((t (:family ,*font-family* :foundry "unknown" :slant normal :weight normal :height ,*font-height* :width normal))))
 '(magit-diff-context-highlight ((t (:inherit nil))))
 '(magit-section-highlight ((t (:inherit highlight)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
