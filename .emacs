; Initialization of packages system
(defun is-osx () (string= "darwin" system-type))

(package-initialize)
(when (is-osx)
  (exec-path-from-shell-initialize))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package cl :ensure t)

; Rest of the config
(defvar *font-family* (if (is-osx)
                          "Monaco-18"
                        "Inconsolata-12"))

(defvar *browser-program* (if (is-osx) "open" "google-chrome"))

(cl-defstruct theme name (background-mode nil))

(defvar *color-themes* (list (make-theme :name 'zenburn :background-mode 'nil)
                             (make-theme :name 'solarized-dark :background-mode 'dark)
                             (make-theme :name 'solarized-light :background-mode 'light)
                             (make-theme :name 'sanityinc-solarized-dark :background-mode 'dark)
                             (make-theme :name 'sanityinc-solarized-light :background-mode 'light))
  "A list of my favourite color themes.")

(cl-defun is-current-color-theme? (theme)
  "Returns t if theme is enabled now."
  (let ((background (theme-background-mode theme)))
    (and (find (theme-name theme) custom-enabled-themes)
         (or (null background)
             (equal background frame-background-mode)))))

(cl-defun get-curr-color-theme (themes)
  "Returns currently-enabled color theme from themes. If there is no
   such theme, returns the first theme from *color-themes*."
  (when (null themes)
    (return-from get-curr-color-theme (first *color-themes*)))
  (let ((theme (first themes)))
    (if (is-current-color-theme? theme)
        theme
      (get-curr-color-theme (cdr themes)))))

(cl-defun get-next-color-theme (themes)
  "Returns the theme after the currently-enabled color theme. If there is no
   such theme, returns the first theme from *color-themes*."
  (when (null themes)
    (cl-return-from get-next-color-theme (first *color-themes*)))
  (let ((theme (first themes)))
    (if (is-current-color-theme? theme)
        (if (null (cdr themes))
            (first *color-themes*)
          (second themes))
      (get-next-color-theme (cdr themes)))))

(cl-defun disable-color-theme (theme)
  (disable-theme (theme-name theme))
  (customize-set-variable 'frame-background-mode nil))

(cl-defun enable-color-theme (theme)
  (let ((name (theme-name theme))
        (background (theme-background-mode theme)))
    (load-theme name t)
    (when background
      (customize-set-variable 'frame-background-mode background))))

(cl-defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file filename new-name 1)
               (rename-buffer new-name))))))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")

(when (is-osx)
  (setq mac-allow-anti-aliasing t))

(cl-defun switch-to-shell ()
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

(cl-defun init-common-edit-mode ()
  (transient-mark-mode 1)
  (global-hl-line-mode 't)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab
        compilation-scroll-output 'first-error
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program *browser-program*)
  (put 'upcase-region 'disabled nil))

(cl-defun customize-common-coding-mode-map (mode-map)
  (define-key mode-map (kbd "C-c C-c") 'recompile)
  (define-key mode-map (kbd "C-c C-r") 'clang-format-region)
  (define-key mode-map (kbd "C-c C-f") 'clang-format-buffer)
  (define-key mode-map (kbd "C-c C-l") 'sort-lines)
  (define-key mode-map (kbd "C-c l") 'sort-lines))

(cl-defun init-common-coding-mode ()
  (use-package clang-format :ensure t)
  (use-package google-c-style
    :ensure t
    :init
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))

  (add-hook 'c-mode-hook (lambda () (customize-common-coding-mode-map c-mode-map)))
  (add-hook 'c++-mode-hook (lambda () (customize-common-coding-mode-map c++-mode-map)))
  (add-hook 'java-mode-hook (lambda () (customize-common-coding-mode-map java-mode-map)))

  (use-package company
    :ensure t
    :diminish company-mode
    :init (global-company-mode t)
    :config
    (setq company-idle-delay 0.5
          company-minimum-prefix-length 2
          company-tooltip-limit 20))

  (use-package ycmd
    :ensure t
    :init
    (add-hook 'c++-mode-hook #'ycmd-mode)
    (add-hook 'c-mode-hook #'ycmd-mode)
    :config
    (set-variable 'ycmd-server-command (list "/usr/bin/python" (file-truename "~/ycmd/ycmd")))
    (set-variable 'ycmd-global-config (file-truename "~/.ycm_extra_conf.py"))
    (set-variable 'ycmd-extra-conf-whitelist (list (file-truename "~/coding/*"))))

  (use-package company-ycmd
    :ensure t
    :init (company-ycmd-setup)))

(cl-defun init-clisp-mode ()
  (use-package slime
    :ensure t
    :config
    (setq slime-lisp-implementations `((sbcl (,(if (is-osx) "/usr/local/bin/sbcl" "/usr/bin/sbcl")) :coding-system utf-8-unix))
          common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
    :bind ([f2] . slime-hyperspec-lookup)))

(cl-defun init-haskell-mode ()
  (use-package haskell-mode
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook (lambda ()
                                   (interactive-haskell-mode)
                                   (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'recompile)))))

(cl-defun init-python-mode ()
  (use-package python-mode
    :init
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "-i")
    :ensure t))

(cl-defun init-helm-mode ()
  (use-package dash :ensure t)

  (use-package helm
    :ensure t
    :init
    (setq helm-display-header-line nil
          helm-split-window-in-side-p t)
    :config
    (helm-mode 1)
    :bind (("C-c h" . helm-command-prefix)
           ("M-x" . helm-M-x)
           ("M-y" . helm-show-kill-ring)
           ("C-x b" . helm-mini)
           ("C-x C-f" . helm-find-files))
    :bind (:map helm-map (("<tab>" . helm-execute-persistent-action)
                          ("C-i" . helm-execute-persistent-action))))

  (global-unset-key (kbd "C-x c"))

  (use-package projectile
    :ensure t
    :init
    (setq projectile-completion-system 'helm
          projectile-use-git-grep t
          projectile-enable-caching t
          projectile-indexing-method 'native)
    :config
    (projectile-global-mode)
    :bind-keymap ("C-c p" . projectile-command-map))

  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)))

(cl-defun init-magit-mode ()
  (use-package magit
    :ensure t
    :bind ("C-c C-g" . magit-status))
  (use-package magit-svn
    :ensure t))

(cl-defun init-writeroom-mode ()
  (use-package writeroom-mode
    :ensure t
    :init
    (setq writeroom-width 120
          writeroom-major-modes '(c++-mode
                                  c-mode
                                  java-mode
                                  python-mode
                                  haskell-mode
                                  lisp-mode
                                  text-mode)
          writeroom-mode-line 't
          writeroom-major-modes-exceptions '(magit-popup-mode magit-log-mode compilation-mode))
    :config
    (global-writeroom-mode 1)))

(cl-defun init-bbdb ()
  (use-package bbdb
    :ensure t
    :init
    (setq bbdb-mua-update-interactive-p '(query . create))
    :config
    (bbdb-initialize 'gnus 'message)
    (bbdb-insinuate-message)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)))

(cl-defun init-doc-view ()
  (setq doc-view-resolution 300))

(cl-defun switch-color-theme ()
  (interactive)
  (let ((curr-theme (get-curr-color-theme *color-themes*))
        (next-theme (get-next-color-theme *color-themes*)))
    (disable-color-theme curr-theme)
    (enable-color-theme next-theme)))

(cl-defun init-color-theme ()
  (use-package color-theme :ensure t)
  (use-package color-theme-sanityinc-solarized :ensure t)
  (use-package color-theme-solarized :ensure t)
  (enable-color-theme (second *color-themes*))
  (global-set-key (kbd "<f11>") 'switch-color-theme))

(cl-defun init-fullscreen ()
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(setq inhibit-splash-screen t)
(init-common-edit-mode)
(init-common-coding-mode)
(init-clisp-mode)
(init-haskell-mode)
(init-python-mode)
(init-helm-mode)
(init-magit-mode)
(init-writeroom-mode)
(init-bbdb)
(init-doc-view)

(when window-system
  (add-to-list 'default-frame-alist `(font . ,*font-family*))
  (set-face-attribute 'default t :font *font-family*)
  (init-fullscreen)
  (init-color-theme))

(shell)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit-svn writeroom-mode use-package slime python-mode magit helm-projectile haskell-mode google-c-style evil-unimpaired company-ycmd clang-format bbdb))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
