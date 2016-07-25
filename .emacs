(require 'cl)

(defun is-osx () (string= "darwin" system-type))

(defvar *packages* '(magit color-theme color-theme-solarized color-theme-sanityinc-solarized
                           clang-format google-c-style
                           haskell-mode go-mode slime helm helm-projectile
                           w3m exec-path-from-shell
                           writeroom-mode))
(defvar *font-family* (if (is-osx)
                          "Monaco-18"
                        "DejaVu Sans Mono-12"))

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

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")

(when (is-osx)
  (setq mac-allow-anti-aliasing t))

(defun init-packages ()
  (package-initialize)
  (when (is-osx)
    (exec-path-from-shell-initialize))
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

  ; On Debian I prefer to use custom clang-format build.
  (unless (is-osx)
    (setq clang-format-executable "~/coding/llvm-cmake-build/bin/clang-format"))

  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (add-hook 'c-mode-hook (lambda () (customize-common-coding-mode-map c-mode-map)))
  (add-hook 'c++-mode-hook (lambda () (customize-common-coding-mode-map c++-mode-map)))
  (add-hook 'java-mode-hook (lambda () (customize-common-coding-mode-map java-mode-map))))

(defun init-clisp-mode ()
  (require 'slime)
  (setq slime-lisp-implementations `((sbcl (,(if (is-osx) "/usr/local/bin/sbcl" "/usr/bin/sbcl")) :coding-system utf-8-unix))
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

(defun init-python-mode ()
  (require 'python-mode)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i"))

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
                                haskell-mode
                                lisp-mode
                                text-mode)
        writeroom-mode-line 't)

  (global-writeroom-mode 1))

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

(when window-system
  (add-to-list 'default-frame-alist `(font . ,*font-family*))
  (set-face-attribute 'default t :font *font-family*)
  (init-fullscreen)
  (init-color-theme))

(shell)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(gnus-group-mail-1 ((t (:foreground "gray53" :weight bold))))
;;  '(gnus-group-mail-3 ((t (:foreground "SkyBlue3" :weight bold))))
;;  '(gnus-group-mail-3-empty ((t (:foreground "DeepSkyBlue4"))))
;;  '(gnus-header-from ((t (:inherit message-header-other-face :foreground "medium violet red" :weight normal))))
;;  '(gnus-header-subject ((t (:inherit message-header-subject :weight normal))))
;;  '(gnus-summary-normal-ticked ((t (:foreground "SteelBlue4"))))
;;  '(magit-diff-context-highlight ((t (:inherit nil))))
;;  '(magit-section-highlight ((t (:inherit highlight)))))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
;;  '(frame-background-mode (quote dark)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#073642")
 '(frame-background-mode nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
