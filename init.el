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

;; Emacs special treatment

(defun find-erlang-emacs-tools ()
  (if (file-directory-p "/usr/lib/erlang")
      (let* ((libs-dir (directory-files "/usr/lib/erlang/lib"))
	     (tools-dir-name (seq-find (lambda (f) (string-match "^tools-*" f)) libs-dir))
	     (tools-full-path (expand-file-name tools-dir-name "/usr/lib/erlang/lib")))
	(expand-file-name "emacs" tools-full-path))
    nil))

(defun init-emacs (tools-dir)
  (setq load-path (cons tools-dir load-path))
  (require 'erlang-start))


(let ((erlang-tools-dir (find-erlang-emacs-tools)))
  (if erlang-tools-dir 
    (init-emacs erlang-tools-dir)
    nil))

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
	evil-collection
	projectile
	neotree
	elixir-mode
	which-key
	magit
	solarized-theme

	;; Completion at point
	company

	;; Helm
	helm
	helm-company
	helm-projectile

	general ;; keybinding macros

	;; Language server
	lsp-mode
	lsp-ui
	flycheck
	dap-mode ;; debugger protocol
	smartparens

	;; Required by evil
	undo-fu)))
  (mapcar 'straight-use-package packages))

(use-package el-patch :straight t)
(require 'use-package)

;; Individual package configuration

(use-package helm
  :bind ("M-x" . 'helm-M-x))

(use-package neotree
  :init (setq
	 neo-keymap-style 'concise
	 neo-smart-open 1)
  :bind (("M-<tab>" . neotree-toggle)
	 ("C-<tab>" . neotree-show)))

(use-package which-key
  :init (setq which-key-popup-type 'minibuffer)
  :config (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :init (setq evil-want-integration t
	      evil-collection-mode-list '(magit neotree dired company ibuffer))
  :config (evil-collection-init))

(use-package projectile
  :config (projectile-mode 1)
  :bind ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))

(use-package company
  :config (global-company-mode))

;; -- UI Resets

;; Relative line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(load-theme 'solarized-light 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-font (font-spec :family "tamsyn" :size 16))

;; Keybindings TODO

(general-define-key
 "C-x C-b" 'ibuffer)

;; open the config
(find-file-existing "~/.emacs.d/init.el")
