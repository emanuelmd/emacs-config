;; -- Straight.el

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; -- Configuration

(setq
      ;; Garbage collect every 10MB
      gc-cons-threshold 10000000             

      ;; Warning when opening large files
      large-file-warning-threshold 100000000 

      ;; Disable backup
      make-backup-files nil

      inhibit-startup-message t
      initial-scratch-message ";; so above as below"
      cursor-type 'bar)

;; Package declaration

(let ((packages
	'(use-package
	evil
	projectile
	helm
	neotree
	elixir-mode
	solarized-theme
	which-key
	;; Required by evil
	undo-fu)))
  (mapcar 'straight-use-package packages))

(use-package el-patch :straight t)
(require 'use-package)
    
(evil-mode 1)

;; Individual package configuration

(use-package neotree
  :init (setq neo-show-hidden-files t)
  :bind ("M-<tab>" . neotree-toggle))

(use-package helm
  :bind ("M-x" . 'helm-M-x))

(use-package evil
  :init (setq evil-undo-system 'undo-fu))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'solarized-light 1)

(set-frame-font (font-spec :family "tamsyn" :size 16))

;; open the config
(find-file-existing "~/.emacs.d/init.el")
