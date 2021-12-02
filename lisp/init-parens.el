;;; init-site-lisp.el --- Support show parens -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(show-paren-mode t) ;;显示匹配的括号
;; https://www.emacswiki.org/emacs/ElectricPair
;; A better solution may be skeleton-insert-pair-maybe (see AutoPairs).
(electric-pair-mode +1)  ;;(electric-pair-local-mode) Toggle ‘electric-pair-mode’ only in this buffer.
(use-package smartparens
  :disabled t
  :init (smartparens-global-mode t)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil) ;; 写lisp的时候不希望成对补全
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))

;;智能括号自动补全 http://www.emacswiki.org/emacs/AutoPairs    ;;(require 'auto-pair+)
(use-package autopair
  :disabled t
  :config
  (autopair-global-mode 1) ;; 全局启用智能补全括号
  (global-set-key (kbd "C-<SPC>") 'forward-char))

;;智能括号匹配高亮 http://www.emacswiki.org/emacs/HighlightParentheses
(use-package highlight-parentheses
  :disabled t
  :config
  ;;使autopair模式和highlight-parentheses集成交互使用
  (add-hook 'highlight-parentheses-mode-hook
        '(lambda ()
           (setq autopair-handle-action-fns
             (append
              (if autopair-handle-action-fns
              autopair-handle-action-fns
            '(autopair-default-handle-action))
              '((lambda (action pair pos-before)
          (hl-paren-color-update)))))))
  (global-highlight-parentheses-mode t)
  (setq show-paren-style 'parentheses))    ;括号匹配显示但不是烦人的跳到另一个括号。

(provide 'init-parens)
;;; init-parens.el ends here
