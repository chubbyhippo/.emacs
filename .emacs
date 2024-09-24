(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents t)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
