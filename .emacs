;; Disable visual bell
(setq ring-bell-function 'ignore)

;; Disable the toolbar
(tool-bar-mode -1)

;; Set up MELPA and ELPA package repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)

;; Ensure use-package is installed
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Undo package
(use-package undo-fu)

;; Evil mode setup
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-C-u-scroll t) ;; Enable C-u scroll in evil mode
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil escape to quickly exit modes
(use-package evil-escape
  :ensure t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jj"
        evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

;; Evil collection
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; Dracula theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Map leader key to space bar
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  ;; Set leader to space
  (evil-leader/set-leader "<SPC>")
  ;; Leader mappings
  (evil-leader/set-key
    "b" (lambda () ;; Open buffer menu
          (interactive)
          (call-interactively #'buffer-menu))
    "ap" (lambda () ;; Open dired (like Lexplore in Vim)
           (interactive)
           (dired "."))
    "aq" (lambda () ;; Open compilation window (like :copen in Vim)
           (interactive)
           (call-interactively #'compilation-start))
    "at" (lambda () ;; Open terminal
           (interactive)
           (ansi-term "/bin/bash")) ;; Replace with your shell if needed
    "dh" (lambda () ;; Clear search highlights
           (interactive)
           (evil-ex-nohighlight))
    "eV" (lambda () ;; Open Emacs configuration
           (interactive)
           (find-file user-init-file))
    "h" (lambda () ;; Close all other windows (like :only in Vim)
           (interactive)
           (delete-other-windows))
    "m" (lambda () ;; Show marks (like :marks in Vim)
           (interactive)
           (evil-show-marks))
    "sf" (lambda () ;; Find a file (like :find in Vim)
           (interactive)
           (call-interactively #'find-file))
    "st" (lambda () ;; Open grep results (like :grep and :copen in Vim combined)
           (interactive)
           (call-interactively #'grep))
    "sV" (lambda () ;; Reload Emacs config
           (interactive)
           (load-file user-init-file))
    "w" (lambda () ;; Save current buffer (like :w in Vim)
           (interactive)
           (save-buffer))
    "q" (lambda () ;; Quit current window (like :q in Vim)
           (interactive)
           (kill-buffer-and-window))
    "Q" (lambda () ;; Force quit (like :q! in Vim)
           (interactive)
           (evil-quit-all-with-error-code))))

;; Custom C-u and C-d behavior in evil mode
(defun my-c-u-and-zz ()
  "Perform C-u action (scroll up) and then call zz."
  (interactive)
  (evil-scroll-up nil)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun my-c-d-and-zz ()
  "Perform C-d action (scroll down) and then call zz."
  (interactive)
  (evil-scroll-down nil)
  (evil-scroll-line-to-center (line-number-at-pos)))

(with-eval-after-load 'evil
  ;; Replace C-u behavior
  (define-key evil-normal-state-map (kbd "C-u") 'my-c-u-and-zz)
  (define-key evil-motion-state-map (kbd "C-u") 'my-c-u-and-zz)

  ;; Replace C-d behavior
  (define-key evil-normal-state-map (kbd "C-d") 'my-c-d-and-zz)
  (define-key evil-motion-state-map (kbd "C-d") 'my-c-d-and-zz))

;; Rebind `n` and `N` to center after jumping to search matches
(with-eval-after-load 'evil
  ;; Rebind `n` to jump to the next search match and center the screen
  (define-key evil-normal-state-map (kbd "n")
    (lambda ()
      (interactive)
      (evil-search-next)
      (evil-scroll-line-to-center (line-number-at-pos))))

  ;; Rebind `N` to jump to the previous search match and center the screen
  (define-key evil-normal-state-map (kbd "N")
    (lambda ()
      (interactive)
      (evil-search-previous)
      (evil-scroll-line-to-center (line-number-at-pos)))))

;; Enable relative line numbers globally
(setq display-line-numbers-type 'relative) ;; Set line numbers to relative
(global-display-line-numbers-mode 1)       ;; Enable line numbers globally

;; Disable line numbers for specific modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
