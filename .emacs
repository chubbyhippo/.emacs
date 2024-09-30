(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)
(package-refresh-contents t)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(evil-define-key 'normal 'global (kbd "C-d")
  (lambda ()
    (interactive)
    (evil-scroll-down nil)
    (recenter)))

(evil-define-key 'normal 'global (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-scroll-up nil)
    (recenter)))

