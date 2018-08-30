(require 'cl)

(defun is-osx () (string= "darwin" system-type))

(defvar *packages* '(magit color-theme color-theme-solarized color-theme-sanityinc-solarized
                           clang-format google-c-style
                           haskell-mode go-mode slime helm helm-projectile
                           w3m exec-path-from-shell
                           writeroom-mode
                           bbdb))
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

(defun init-packages ()
  (package-initialize)
  (when (is-osx)
    (exec-path-from-shell-initialize))
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
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
        browse-url-generic-program *browser-program*)
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

  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (add-hook 'c-mode-hook (lambda () (customize-common-coding-mode-map c-mode-map)))
  (add-hook 'c++-mode-hook (lambda () (customize-common-coding-mode-map c++-mode-map)))
  (add-hook 'java-mode-hook (lambda () (customize-common-coding-mode-map java-mode-map)))

  (use-package irony
               :ensure t
               :defer t
               :init
               (add-hook 'c++-mode-hook 'irony-mode)
               (add-hook 'c-mode-hook 'irony-mode)
               :config
               ;; replace the `completion-at-point' and `complete-symbol' bindings in
               ;; irony-mode's buffers by irony-mode's function
               (defun my-irony-mode-hook ()
                 (define-key irony-mode-map [remap completion-at-point]
                   'irony-completion-at-point-async)
                 (define-key irony-mode-map [remap complete-symbol]
                   'irony-completion-at-point-async))
               (add-hook 'irony-mode-hook 'my-irony-mode-hook)
               (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package company
               :ensure t
               :defer t
               :init (add-hook 'after-init-hook 'global-company-mode)
               :config
               (use-package company-irony :ensure t :defer t)
               (setq company-minimum-prefix-length   2
                     company-show-numbers            t
                     company-tooltip-limit           20
                     company-dabbrev-downcase        nil
                     company-backends                '((company-irony)))
               :bind ("C-;" . company-complete-common)))

(defun init-clisp-mode ()
  (require 'slime)
  (setq slime-lisp-implementations `((sbcl (,(if (is-osx) "/usr/local/bin/sbcl" "/usr/bin/sbcl")) :coding-system utf-8-unix))
        common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  (global-set-key [(f2)] 'slime-hyperspec-lookup))

(defun init-haskell-mode ()
  (require 'haskell-mode)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (turn-on-haskell-indent)
                                 (define-key haskell-mode-map (kbd "C-c C-c") 'recompile)))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(defun init-go-mode ()
  (require 'go-mode)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  (define-key go-mode-map (kbd "C-c C-c") 'recompile)
  (define-key go-mode-map (kbd "C-c C-p") 'switch-to-shell)
  (define-key go-mode-map (kbd "C-c p") 'switch-to-shell))

(defun init-python-mode ()
  (require 'ein)
  (require 'python-mode)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i"))

(defun init-helm-mode ()
  (require 'dash)
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
        projectile-use-git-grep t
        projectile-enable-caching t
        projectile-indexing-method 'native)
  (helm-projectile-on))

(defun init-magit-mode ()
  (require 'magit)
  (global-set-key (kbd "C-c C-g") 'magit-status))

(cl-defun init-writeroom-mode ()
  (setq writeroom-width 100
        writeroom-major-modes '(c++-mode
                                c-mode
                                java-mode
                                python-mode
                                haskell-mode
                                lisp-mode
                                text-mode)
        writeroom-mode-line 't
        writeroom-major-modes-exceptions '(magit-popup-mode magit-log-mode compilation-mode))
  (global-writeroom-mode 1))

(cl-defun init-bbdb ()
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-insinuate-message)
  (setq bbdb-mua-update-interactive-p '(query . create))
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

(cl-defun init-doc-view ()
  (setq doc-view-resolution 300))

(defun switch-color-theme ()
  (interactive)
  (let ((curr-theme (get-curr-color-theme *color-themes*))
        (next-theme (get-next-color-theme *color-themes*)))
    (disable-color-theme curr-theme)
    (enable-color-theme next-theme)))

(defun init-color-theme ()
  (require 'color-theme)
  (require 'color-theme-sanityinc-solarized)
  (enable-color-theme (second *color-themes*))
  (global-set-key (kbd "<f11>") 'switch-color-theme))

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
