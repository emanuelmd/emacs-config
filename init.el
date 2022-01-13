
;; Load core modules

(defvar configuration-dir (expand-file-name "lib" (file-name-directory load-file-name))
  "Configuration directory.")

(add-to-list 'load-path configuration-dir)

(require 'core)
(require 'packages)
(require 'ui)

;; Open the config
(find-file-existing "~/.emacs.d/lib/core.el")
