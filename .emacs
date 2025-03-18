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

;; Evil collection
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;; Evil escape to quickly exit modes
(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jj")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode 1))

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
  ;; Map <leader>b to :buffers<cr>
  (evil-leader/set-key
    "b" (lambda ()
          (interactive)
          (call-interactively #'buffer-menu))))

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
