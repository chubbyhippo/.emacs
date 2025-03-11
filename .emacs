;; Initialize package sources
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install `use-package` if not available
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Automatically install missing packages

;; Enable `evil` (Vim-like editing)
(use-package evil
  :config
  (evil-mode 1)

  ;; Configure "jj" as an alternative for ESC
  (defun my-evil-insert-mode-jj-escape ()
    "Map 'jj' to exit insert mode in Evil."
    (interactive)
    (let ((key-sequence (this-command-keys)))
      (if (string= key-sequence "jj")
          (evil-normal-state)
        (self-insert-command 1))))
  (define-key evil-insert-state-map "j" #'my-evil-insert-mode-jj-escape))

;; Enable `cider` for Clojure development
(use-package cider
  :commands (cider-jack-in cider-connect)
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only
        cider-show-error-buffer 'only-in-repl
        cider-auto-select-error-buffer nil
        cider-repl-wrap-history t
        cider-repl-display-help-banner nil))

;; Configure additional Clojure development features
(use-package clojure-mode)
(use-package paredit
  :hook (clojure-mode . paredit-mode)) ;; Enable Paredit for Clojure
(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode)) ;; Highlight nested parentheses

;; Optional: Enhanced Evil integration with Paredit
(defun my/setup-evil-paredit ()
  "Add Evil bindings for Paredit operations."
  (define-key evil-normal-state-map (kbd "M-(") 'paredit-wrap-round)
  (define-key evil-normal-state-map (kbd "M-[") 'paredit-wrap-square)
  (define-key evil-normal-state-map (kbd "M-{") 'paredit-wrap-curly))
(add-hook 'paredit-mode-hook 'my/setup-evil-paredit)

;; Visual improvements
(menu-bar-mode -1)  ;; Disable menu bar
(tool-bar-mode -1)  ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable scroll bar
(column-number-mode 1) ;; Show column numbers
(global-display-line-numbers-mode 1) ;; Show line numbers globally

;; Load a theme (e.g., Dracula)
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; Keep packages up to date using auto-package-update
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

;; Save desktop sessions
(desktop-save-mode 1)
