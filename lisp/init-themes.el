;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/emacs-jp/replace-colorthemes/
;; (use-package color-theme-modern
;;   :ensure t
;;   :demand
;;   :init
;;   (setq theme-name 'desert )  ;; 'comidia  , 'clarity , 'desert
;;   :config
;;   (load-theme theme-name t t)
;;   (enable-theme theme-name))

;; (use-package gruvbox-theme
;;     :init (load-theme 'gruvbox-dark-soft t))
(use-package gruvbox-theme)
;; https://zhuanlan.zhihu.com/p/341512250
;; 顺便配置一个好看一点的Mode Line。
;; (use-package smart-mode-line
;;     :init
;;     (setq sml/no-confirm-load-theme t)
;;     (setq sml/theme 'respectful)
;;     (sml/setup))

;; (use-package doom-themes
;;   :disabled t
;;   :init
;;   (setq doom-themes-enable-bold t
;;     doom-themes-enable-italic t)
;;   :config
;;   (load-theme 'doom-one t))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)
;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(gruvbox-dark-soft))

;; Ensure that themes will be applied even if they have not been customized
(defun init-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))


(provide 'init-themes)
;;; init-themes.el ends here
