;; Disable visual bell to avoid irritating blinking
(setq ring-bell-function 'ignore)

;; Disable the toolbar for a cleaner UI
(tool-bar-mode -1)

;; Set up MELPA and ELPA package repositories for package installation
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)

;; Ensure `use-package` is installed for easier package management
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Undo package: Provides enhanced undo functionality
(use-package undo-fu)

;; Evil mode setup (Vim emulation in Emacs)
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)) ;; Escape quits matches, buffers, etc.
  :init
  ;; Customize Evil's behavior
  (setq evil-want-C-u-scroll t)  ;; Enable C-u scroll in Evil mode
  (setq evil-undo-system 'undo-fu) ;; Use `undo-fu` as the undo system for Evil
  (setq evil-want-keybinding nil)  ;; Avoid conflicts with `evil-collection`
  :config
  ;; Enable Evil mode globally
  (evil-mode 1))

;; Quickly exit modes (like pressing "jj" for escape in Vim)
(use-package evil-escape
  :ensure t
  :init
  ;; Customize `evil-escape` behavior
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion) ;; Disable escape for some states
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode) ;; Disable for specific modes
        evil-escape-key-sequence "jj" ;; Use "jj" as the escape shortcut
        evil-escape-delay 0.2) ;; Time to detect "jj"
  :config
  ;; Enable Evil escape globally
  (evil-escape-mode 1))

;; Evil collection: Integrates Evil with other Emacs packages
(use-package evil-collection
  :after evil
  :config
  ;; Enable integration with various modes
  (setq evil-want-integration t)
  (evil-collection-init))

;; Dracula theme (dark color scheme for better readability)
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)) ;; Load Dracula theme

;; Evil-Leader configuration (space bar as the leader key)
(use-package evil-leader
  :ensure t
  :config
  ;; Enable Evil-Leader globally
  (global-evil-leader-mode)

  ;; Set Leader to Space ("<SPC>")
  (evil-leader/set-leader "<SPC>")

  ;; Define global leader mappings
  (evil-leader/set-key
    "b" #'buffer-menu               ;; Open buffer menu
    "ap" #'dired                    ;; Open Dired (file explorer)
    "aq" #'compilation-start        ;; Start a compilation process
    "at" #'ansi-term                ;; Open terminal (default to bash)
    "dh" #'evil-ex-nohighlight      ;; Clear search highlights
    "eV" #'(lambda ()               ;; Open Emacs configuration
             (interactive)
             (find-file user-init-file))
    "h" #'delete-other-windows      ;; Close all other windows
    "m" #'evil-show-marks           ;; Show Evil marks
    "sf" #'find-file                ;; Find a file
    "st" #'grep                     ;; Run Grep and show results
    "sV" #'(lambda ()               ;; Reload Emacs config
             (interactive)
             (load-file user-init-file))
    "w" #'save-buffer               ;; Save current buffer
    "q" #'kill-buffer-and-window    ;; Quit current window
    "Q" #'evil-quit-all-with-error-code) ;; Force quit Emacs

  ;; Define mode-specific leader bindings for Emacs Lisp files
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "rr" #'eval-last-sexp)) ;; Run last s-expression (`C-x C-e` equivalent)

;; Custom scrolling behavior for `C-u` and `C-d`, with centering
(defun my-c-u-and-zz ()
  "Scroll up and center the screen."
  (interactive)
  (evil-scroll-up nil)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun my-c-d-and-zz ()
  "Scroll down and center the screen."
  (interactive)
  (evil-scroll-down nil)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; Apply custom scrolling behavior in Evil mode
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-u") #'my-c-u-and-zz) ;; Override C-u
  (define-key evil-motion-state-map (kbd "C-u") #'my-c-u-and-zz)
  (define-key evil-normal-state-map (kbd "C-d") #'my-c-d-and-zz) ;; Override C-d
  (define-key evil-motion-state-map (kbd "C-d") #'my-c-d-and-zz))

;; Automatically center the screen after search jumps (`n` and `N`)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "n")
    (lambda ()
      "Jump to the next search match and center the screen."
      (interactive)
      (evil-search-next)
      (evil-scroll-line-to-center (line-number-at-pos))))
  (define-key evil-normal-state-map (kbd "N")
    (lambda ()
      "Jump to the previous search match and center the screen."
      (interactive)
      (evil-search-previous)
      (evil-scroll-line-to-center (line-number-at-pos)))))

;; Enable relative line numbers globally
(setq display-line-numbers-type 'relative) ;; Set line numbers to relative
(global-display-line-numbers-mode 1)       ;; Enable globally

;; Disable line numbers for specific modes (e.g., terminal, Dired)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
