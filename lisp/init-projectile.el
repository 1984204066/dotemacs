;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))


;; (use-package projectile
;;   ;; https://github.com/jwiegley/use-package#extending-the-load-path
;;   :bind-keymap ("C-p" . projectile-command-map)  ;;default is?.
;;   :init (setq projectile-use-git-grep t)
;;   :config
;;   (projectile-global-mode)
;;   ;; Shorter modeline
;;   (setq-default projectile-mode-line-prefix " Proj")
;;   (when (executable-find "rg")
;;     (setq-default projectile-generic-command "rg --files --hidden"))
;;   ;; (projectile-mode t)
;;   ;; (setq ;projectile-cache-file (concat mage-cache-dir "projectile.cache")
;;   ;;   projectile-enable-caching t
;;   ;;   projectile-indexing-method 'alien
;;   ;;   ;projectile-known-projects-file (concat mage-cache-dir "projectile.projects")
;;   ;;   projectile-require-project-root t
;;   ;;   projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".Po")
;;   ;;   ;;projectile-globally-ignored-directories '(".deps")
;;   ;;   projectile-completion-system 'helm ;;经常用ivy https://www.jianshu.com/p/250301ebb7f9
;;   ;;   )
;;   ;; (append '(".deps") projectile-globally-ignored-directories)
;;   (use-package helm-projectile)
;;   (use-package ibuffer-projectile))

(provide 'init-projectile)
;;; init-projectile.el ends here
