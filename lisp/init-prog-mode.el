;;; init-prog-mode.el --- init program related stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package init-projectile
  :defer nil
  :load-path "./lisp"
  ;; :hook (after-init . 'projectile-mode)
  )

(use-package web-mode
  :mode ("\\.html?\\'" "\\.phtml\\'" ))
(add-hook 'js-mode-hook (lambda()(interactive)
			  (setq tab-width 4
				default-tab-width 4
				indent-tabs-mode nil)
			  ))
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save ;;need rustfmt.toml edition=2021
  (setq rustic-format-on-save t)
  ;; :hook (before-save . delete-trailing-whitespace)
  :hook (rustic-mode . rk/rustic-mode-hook))

;; https://github.com/brotzeit/rustic/issues/253
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
    (if buffer-file-name
        (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package company
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package yasnippet
  :config (yas-reload-all)
  :hook ((prog-mode text-mode) . 'yas-minor-mode))

;;https://github.com/AndreaCrotti/yasnippet-snippets/tree/master
(use-package yasnippet-snippets
  :config (require 'yasnippet))

(use-package flycheck
  ;; :hook (prog-mode . flycheck-mode) ;;如果你只想在编程语言的模式下启用
  :hook (after-init . global-flycheck-mode) ;; 建议全局启用
  )

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dap-ui
  :load-path "./lisp/dap-mode"
  )

(use-package dap-mode
  :load-path "./lisp/dap-mode"
  :config
  ;; (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

;; (use-package astyle
;;   :load-path "./lisp/astyle"
;;   :when (executable-find "astyle")
;;   :init (use-package reformatter)
;;   :hook (c-mode-common . astyle-on-save-mode))
(use-package format-all
  :demand
  :load-path "./lisp/format-all-the-code")

(use-package init-cc-mode
  :load-path "./lisp")

(provide 'init-prog-mode)
;;; init-prog-mode.el ends here
