;;; init-cc-mode.el ---  CC-mode配置  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://www.cnblogs.com/lance-ehf/p/4449410.html
;; http://cc-mode.sourceforge.net/
;; https://pastebin.com/a13ZXDjB
(use-package cc-mode
  :mode ("\\.h\\'" . c-mode)
  :mode ("\\.hpp\\'" . c++-mode)
  :requires irony
  ;;按键定义
  :bind (:map c-mode-base-map
	      ([(control \`)] . hs-toggle-hiding)
	      ([(return)] . newline-and-indent)
	      ([(C-f7)] . compile)
	      ([(meta \`)] . c-indent-command)
	      ;; ([(tab)] . hippie-expand)
	      ;; ([(tab)] . my-indent-or-complete)
	      )
  ;; (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; :hook (c-mode-common . 'my-c-mode-common-hook)
  ;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  :config
  (c-set-offset 'inline-open 0)
  (c-set-offset 'friend '-)
  (c-set-offset 'substatement-open 0)
  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-basic-offset 4)
  ;;; hungry-delete and auto-newline
  (c-toggle-auto-hungry-state 1)
  ;;预处理设置
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)

  (irony-mode)
;;  (ggtags-mode 1)
  (add-to-list (make-local-variable 'company-backends)
               '(company-irony-c-headers company-irony company-yasnippet))

  (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)

  (use-package clang-format
      :ensure t
      :init
      :bind
      ("C-c C-r" . clang-format-region))
)


(use-package irony
  :init
  (setq irony-server-install-prefix
	(expand-file-name (concat user-emacs-directory "servers/Irony")))
  :bind (:map irony-mode-map
	      ([remap completion-at-point] . irony-completion-at-point-async)
	      ([remap complete-symbol] . irony-completion-at-point-async)
	      )
  :config
  (progn
    (use-package flycheck-irony
      :ensure t
      :defer 2
      :hook (flycheck-mode . flycheck-irony-setup)
      )
    (irony-cdb-autosetup-compile-options)
    (company-irony-setup-begin-commands))
  )

;; http://tuhdo.github.io/c-ide.html
;; (use-package ggtags
;;   :ensure t
;;   :bind (:map ggtags-mode-map
;;   ("C-c g s" . 'ggtags-find-other-symbol)
;;   ("C-c g h" . 'ggtags-view-tag-history)
;;   ("C-c g r" . 'ggtags-find-reference)
;;   ("C-c g f" . 'ggtags-find-file)
;;   ("C-c g c" . 'ggtags-create-tags)
;;   ("C-c g u" . 'ggtags-update-tags))
;;   :config  ;;(ggtags-mode 1) 好像没啥用, 必须加到 c-mode-common-hook里面
;;   (add-hook 'c-mode-common-hook
;;         (lambda ()
;;           (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;         (ggtags-mode 1)))))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
