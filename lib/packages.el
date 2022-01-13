(straight-use-package 'use-package)

(use-package el-patch :straight t)

(require 'use-package)

;; Declaration

(straight-use-package 'evil) 
(straight-use-package 'projectile)
(straight-use-package 'helm)
(straight-use-package 'neotree)
(straight-use-package 'elixir-mode)

(evil-mode 1)

;; Configuration

(use-package neotree
  :init (setq neo-show-hidden-files t)
  :bind ("M-<tab>" . neotree-toggle))

(use-package helm
  :bind ("M-x" . 'helm-M-x))

(provide 'packages)

