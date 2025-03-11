;; Initialize package sources
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Ensure package sources are initialized
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Helper function for installing packages if not available
(defun ensure-package-installed (&rest packages)
  "Ensure PACKAGES are installed. Install missing ones from package archives."
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        packages))

;; Install and configure `evil` (Vim-like editing)
(ensure-package-installed 'evil)
(require 'evil)
(evil-mode 1)

;; Configure "jj" as an alternative to `ESC`
(defun my-evil-insert-mode-jj-escape ()
  "Map 'jj' to escape insert mode in Evil."
  (interactive)
  (let ((key-sequence (this-command-keys)))
    (if (string= key-sequence "jj")
        (evil-normal-state)
      (self-insert-command 1))))
(define-key evil-insert-state-map "j" #'my-evil-insert-mode-jj-escape)

;; Install and configure `cider` (Clojure development)
(ensure-package-installed 'cider)

;; Optional: Additional features for editing experience in Clojure
(ensure-package-installed 'clojure-mode 'paredit 'rainbow-delimiters)

;; Enable Paredit for Lisp-style structured editing
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Enable Rainbow Delimiters for better visual of parentheses
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; Optional Evil integration with Paredit (useful for seamless Vim/Paredit functionality)
(defun my/setup-evil-paredit ()
  "Set up Evil for working with Paredit."
  (define-key evil-normal-state-map (kbd "M-(") 'paredit-wrap-round)
  (define-key evil-normal-state-map (kbd "M-[") 'paredit-wrap-square)
  (define-key evil-normal-state-map (kbd "M-{") 'paredit-wrap-curly))
(add-hook 'paredit-mode-hook 'my/setup-evil-paredit)

;; Cider configuration
(setq cider-repl-pop-to-buffer-on-connect 'display-only) ;; Non-intrusive REPL connect
(setq cider-show-error-buffer 'only-in-repl) ;; Show error buffers only in REPL
(setq cider-auto-select-error-buffer nil) ;; Don't auto-select error buffers
(setq cider-repl-wrap-history t) ;; Wrap REPL history
(setq cider-repl-display-help-banner nil) ;; Disable help banner on REPL start

;; Keybindings for Cider
(with-eval-after-load 'cider
  (define-key cider-mode-map (kbd "C-c C-j") 'cider-jack-in)
  (define-key cider-mode-map (kbd "C-c C-k") 'cider-load-buffer))

;; General Emacs UI/UX improvements
(menu-bar-mode -1)  ;; Disable menu bar
(tool-bar-mode -1)  ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable scroll bar
(column-number-mode 1) ;; Show column numbers
(global-display-line-numbers-mode 1) ;; Show line numbers globally

;; Theme (Optional)
(ensure-package-installed 'dracula-theme)
(load-theme 'dracula t)

;; Save session on exit
(desktop-save-mode 1)

;; Enable automatic package cleanup for dependencies
(ensure-package-installed 'auto-package-update)
(require 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)
