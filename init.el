;;; init.el --- Emacs configuration of Sviatoslav Bulbakha

;; Coypright (C) 2018 Sviatoslav Bulbakha

;; URL: https://github.com/ssbb/.emacs.d

;;; Commentary:

;; This is the GNU Emacs configuration of Sviatoslav Bulbakha, mostly used for
;; Elixir, Elm, Python and Org mode.

;;; Code:


;;
;;; User Info
;;

(setq user-full-name "Sviatoslav Bulbakha"
      user-mail-address "mail@ssbb.me")

;;
;;; Package Management
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;;
;;; Bootstrap `use-package'
;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;
;;; User interface
;;

;; disable splash screen
(setq inhibit-startup-message t
      inhibit-default-init t
      initial-scratch-message nil)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Scroll bar, Tool bar, Menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Use system PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Backup
(setq make-backup-files nil)

;; Better y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Kill current buffer without prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

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
  (setq helm-quick-update t)
  (setq helm-buffers-fuzzy-matching t)
  (helm-mode 1))

;; Projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line-function (lambda () (format " P[%s]" (projectile-project-name))))
  (projectile-mode))

;; Projectile + Helm integration
(use-package helm-projectile
  :config
  (setq projectile-enable-caching t)
  (helm-projectile-on))

;; Using Silver Searchger for search within a project
(use-package helm-ag
  :after helm)

(use-package helm-rage
  :ensure t
  ;:load-path "/data/home/phaun/development/projects/helm-rage"
  :defer t
  :init
  (define-key global-map (kbd "C-c r") 'helm-rage)
  :after helm)

;; Line numbers
(require 'display-line-numbers)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; No Tabs
(setq-default indent-tabs-mode nil)

;; Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

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
  :no-require t
  :config
  (setq winum-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
	  (define-key map (kbd "C-²") 'winum-select-window-by-number)
	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	  (define-key map (kbd "M-1") 'winum-select-window-1)
	  (define-key map (kbd "M-2") 'winum-select-window-2)
	  (define-key map (kbd "M-3") 'winum-select-window-3)
	  (define-key map (kbd "M-4") 'winum-select-window-4)
	  (define-key map (kbd "M-5") 'winum-select-window-5)
	  (define-key map (kbd "M-6") 'winum-select-window-6)
	  (define-key map (kbd "M-7") 'winum-select-window-7)
	  (define-key map (kbd "M-8") 'winum-select-window-8)
	  map))
  (require 'winum)
  (winum-mode))

;;;; PROGRAMMING LANGUAGES

;; Web-mode
(use-package web-mode
  :mode ("\\.html\\'" "\\.eex\\'")
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))

  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode)))

;; CSS & JS indent
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Better RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Elm langauge
(use-package elm-mode)

(use-package flycheck-elm
  :after (elm-mode flycheck)
  :hook (flycheck-mode . flycheck-elm-setup)
  :config
  (add-hook 'css-mode-hook (lambda () (flycheck-mode -1))))

;; Elixir language
(use-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;; (use-package flycheck-mix
;;   :config
;;   (flycheck-mix-setup))

(use-package alchemist
  :after (elixir-mode))

;; Much better commenting
(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))

;; Trim whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(global-subword-mode 1)

;; Python setup
(use-package elpy
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package py-autopep8
  :hook (elpy-mode . py-autopep8-enable-on-save))

(use-package flycheck-pycheckers
  :after (flycheck)
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(defun org-archive-tasks (state)
  (interactive "s Enter State: ")
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   (concat "/" state) 'tree))

;; recentf
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

(use-package yaml-mode)

(use-package pyenv-mode)

(use-package pyenv-mode-auto
  :after pyenv-mode)

(use-package reason-mode
  :config
  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook #'refmt-before-save))))

(use-package rjsx-mode)

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'elm-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook (lambda () (flycheck-mode -1))))

(use-package hindent
  :config
  (setq hindent-reformat-buffer-on-save t)
  (add-hook 'haskell-mode-hook #'hindent-mode))

;;;; CUSTOM FILE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-sass-executable "/Users/ssbb/.gems/bin/scss")
 '(flycheck-sass/scss-sass-lint-executable "/Users/ssbb/.gems/bin/scss")
 '(package-selected-packages
   (quote
    (hindent flycheck-haskell haskell-mode prettier-js rjsx-mode reason-mode sqlformat sql-indent pyenv-mode yaml-mode helm-rage flycheck-mix emmet-mode keyfreq flycheck-pycheckers elpy py-autopep8 expand-region yasnippet-snippets yasnippet helm-ag evil-nerd-commenter com-css-sort smartparens alchemist winum web-mode magit doom-modeline flycheck-color-mode-line flycheck-pos-tip flycheck-elm exec-path-from-shell flycheck elm-mode helm-projectile projectile helm color-theme-sanityinc-tomorrow use-package)))
 '(sqlformat-command (quote pgformatter)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
