;;; init.el --- Emacs configuration of Sviatoslav Bulbakha

;; Coypright (C) 2018 Sviatoslav Bulbakha

;; URL: https://github.com/ssbb/.emacs.d

;;; Commentary:

;; This is the GNU Emacs configuration of Sviatoslav Bulbakha, mostly used for
;; Elixir, Elm, Python and Org mode.

;;; Code:


(setq user-full-name "Sviatoslav Bulbakha"
      user-mail-address "mail@ssbb.me")

;; Package Management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Splash Screen
(setq inhibit-startup-message t
      inhibit-default-init t
      initial-scratch-message nil)

;; Disable audio bell
 (setq ring-bell-function 'ignore)

;; Scroll bar, Tool bar, Menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Use system PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Backup
(setq make-backup-files nil)

;; Better y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
(set-face-attribute 'default nil
		    :family "Iosevka"
		    :height 130
		    :weight 'normal
		    :width 'normal)

;; Theme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;; Helm
(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-buffers-list)
	 ("C-c r" . helm-recentf))
  :config
  (require 'helm-config)
  (helm-mode 1))

;; Projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

;; Projectile + Helm integration
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; Line numbers
(require 'display-line-numbers)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Highlight parens
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after (flycheck)
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flycheck-color-mode-line
  :after (flycheck)
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-init))

;; Magit
(use-package magit
  :bind (("C-c g" . magit)))

;; Window numbering
(use-package winum
  :config
  (winum-mode))

;;;; PROGRAMMING LANGUAGES

;; Web-mode
(use-package web-mode
  :mode "\\.html\\'")

;; Elm langauge
(use-package elm-mode
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :after (elm-mode flycheck)
  :hook (flycheck-mode . flycheck-elm-setup))

;;;; CUSTOM FILE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (winum web-mode magit doom-modeline flycheck-color-mode-line flycheck-pos-tip flycheck-elm exec-path-from-shell flycheck elm-mode helm-projectile projectile helm color-theme-sanityinc-tomorrow use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
