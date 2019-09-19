;;; init.el --- Emacs configuration of Sviatoslav Bulbakha

;; Coypright (C) 2018 Sviatoslav Bulbakha

;; URL: https://github.com/ssbb/.emacs.d

;;; Commentary:

;; This is the GNU Emacs configuration of Sviatoslav Bulbakha, mostly used for
;; Elixir, Elm, Python and Org mode.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  (add-to-list 'load-path "~/.emacs.d/vendor/")
  

  ;; Set repositories
  (require 'package)
  
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Install dependencies
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (setq-default
   use-package-always-ensure t)

  ;; Use latest Org
  (use-package org :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))


;;; init.el ends here
