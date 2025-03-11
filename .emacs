(setq ring-bell-function 'ignore)

;; Set font family, size, and line height
(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :height 130) ;; Font size 13pt (height is in 10ths of a point)
;; Set line spacing (similar to line height)
(setq-default line-spacing 0.2) ;; Adjust for 1.2 line height

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package undo-fu)

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jj")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode 1))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))