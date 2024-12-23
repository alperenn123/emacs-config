(require 'package)

;; Add MELPA and initialize package manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Basic UI and behavior settings
(setq inhibit-startup-message t)         ;; Hide startup message
(tool-bar-mode -1)                       ;; Disable tool bar
(scroll-bar-mode -1)                     ;; Disable scroll bar
(menu-bar-mode -1)                       ;; Disable menu bar
(show-paren-mode 1)                      ;; Highlight matching parentheses
(column-number-mode 1)                   ;; Show column numbers
(global-display-line-numbers-mode t)     ;; Enable line numbers globally
(setq visible-bell t)                    ;; Use visible bell instead of sound
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize on startup

;; Package management with `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Evil Mode for Vim keybindings
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") 'ignore))

;; Tree-sitter for syntax highlighting
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (dolist (hook '(go-mode-hook
                  c-mode-hook
                  c++-mode-hook
                  js-mode-hook
                  typescript-mode-hook
                  html-mode-hook
                  css-mode-hook
                  json-mode-hook
                  yaml-mode-hook
                  python-mode-hook
                  sh-mode-hook
                  rjsx-mode-hook
                  js2-mode-hook))
    (add-hook hook #'tree-sitter-mode)
    (add-hook hook #'tree-sitter-hl-mode)))

;; Theme and font
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))
(set-frame-font "Hack Nerd Font Mono Regular-11" nil t)

;; `smex` for improved M-x experience
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

;; Set up basic editing preferences
(setq-default indent-tabs-mode nil      ;; Use spaces instead of tabs
              tab-width 2               ;; Tab width of 2 spaces
              standard-indent 2)        ;; Standard indent width of 2 spaces

;; Configure line numbers
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type 'relative))) ;; Relative line numbers for code
(add-hook 'text-mode-hook (lambda () (setq display-line-numbers-type nil)))        ;; No line numbers for text

;; Configure `ido` for flexible minibuffer completion
(use-package ido
  :ensure t
  :init
  (ido-mode 1)
  (ido-everywhere 1))

;; Enable `windmove` for easy window navigation
(windmove-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(iedit multiple-cursors smex tree-sitter-langs solarized-theme projectile helm go-mode evil ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36")))))

;; Optional: Keybinding for `execute-extended-command`
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package ag
  :ensure t
  :config
  ;; Optional: Customize `ag` behavior
  (setq ag-highlight-search t        ;; Highlight matches
        ag-reuse-buffers t           ;; Use the same search buffer
        ag-reuse-window t))          ;; Use the same window for results
(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'alien   ;; Use external tools like `ag`
        projectile-generic-command "ag -0 -l --nocolor --hidden"))
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p s g") 'projectile-ag)
