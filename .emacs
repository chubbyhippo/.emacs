(electric-pair-mode 1)
(setq-default display-line-numbers 'relative)
(setq visible-bell 1)
(windmove-default-keybindings)

(setq viper-mode t)
(require 'viper)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(setq viper-ex-style-editing nil)
(setq viper-no-multiple-ESC nil)
(setq viper-syntax-preference 'enxtended)
(setq viper-vi-style-in-minibuffer nil)
(setq viper-want-ctl-h-help t)
(define-key viper-vi-basic-map (kbd "v") 'set-mark-command)
(define-key viper-vi-basic-map (kbd "g") 'universal-argument)

(define-key viper-vi-basic-map (kbd "C-b") nil)
(define-key viper-vi-basic-map (kbd "C-e") nil)
(define-key viper-vi-basic-map (kbd "C-f") nil)

(add-hook 'viper-insert-state-hook
	  (lambda ()
            (local-set-key (kbd "C-u") 'universal-argument)))
