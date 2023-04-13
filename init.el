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

;; erlang special treatment

(defun find-erlang-emacs-tools (search-path)
  (when-let* ((dir-exists (file-directory-p search-path))
        (libs-dir (expand-file-name "lib/" search-path))
	    (tools-dir (seq-find (lambda (f) (string-match "^tools-*" f)) (directory-files libs-dir)))
	    (tools-full-path (expand-file-name tools-dir libs-dir)))
	(expand-file-name "emacs" tools-full-path)))

(defun init-erlang (tools-dir)
  (add-to-list 'load-path tools-dir)
  (require 'erlang-start))

(if-let (erlang-tools-dir (find-erlang-emacs-tools "/home/emanuel/.asdf/installs/erlang/24.3/"))
    (init-erlang erlang-tools-dir)
    (display-warning :warning "Could not find erlang directory"))

;; -- Configuration

(setq-default indent-tabs-mode nil
	      tab-always-indent nil
	      tab-width 2

	      fill-column 80
	      word-wrap t)

(setq
      ;; Garbage collect every 10MB
      gc-cons-threshold 10000000             

      ;; Warning when opening large files
      large-file-warning-threshold 100000000 

      ;; Disable backup
      make-backup-files nil

      ;; Disable autosave
      auto-save-default nil

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
	which-key
	magit
	solarized-theme

	;; Languages

	elixir-mode

	typescript-mode

	;; Helm
	helm
	helm-projectile
	helm-rg

	general ;; Keybinding macros

	smartparens

  ;; Other

  docker
  dockerfile-mode
  swiper
  yaml-mode

  exec-path-from-shell

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
	 neo-smart-open 1
     neo-create-file-auto-open 1
     neo-window-width 35
	 neo-window-fixed-size nil)
  :bind (("M-<tab>" . neotree-toggle)
	 ("C-<tab>" . neotree-show)))

(use-package which-key
  :init (setq which-key-popup-type 'minibuffer)
  :config (which-key-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config (evil-mode +1))

(use-package evil-collection
  :after evil
  :init (setq evil-want-integration t
	      evil-collection-mode-list '(magit neotree dired ibuffer))
  :config (evil-collection-init))

(use-package projectile
  :config (projectile-mode 1)
  :bind ("C-c p" . projectile-command-map)
  :init (setq projectile-enable-caching 1))

(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))

(use-package helm-rg
  :after helm
  :custom-face
  (helm-rg-file-match-face ((t (:foreground "black" :underline t)))))

(use-package smartparens

  :config

  (require 'smartparens-config)

  (setq sp-highlight-pair-overlay nil
	sp-highlight-wrap-overlay nil
	sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)

  (add-hook 'eval-expression-minibuffer-setup-hook
	    (lambda () (when smartparens-global-mode (smartparens-mode +1))))

  (add-hook 'minibuffer-setup-hook
	    (lambda () (when (and smartparens-global-mode (memq this-command '(evil-ex)))
			 (smartparens-mode + 1))))

  (smartparens-global-strict-mode +1))

(use-package docker
  :bind ("C-c d" . docker))

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(exec-path-from-shell-initialize)

;; -- UI Resets

;; Relative line number
(setq display-line-numbers-type 'relative)
(setq inhibit-splash-screen t)
(global-display-line-numbers-mode)

(load-theme 'solarized-light 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-font (font-spec :family "tamsyn" :size 20))

(general-define-key
 "C-x C-b" 'ibuffer
 "C-x b" 'ibuffer
 "C-c p a" 'projectile-add-known-project
 "C-s" 'swiper)

;; open the config
(find-file-existing "~/.emacs.d/init.el")

