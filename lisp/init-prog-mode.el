;;; init-prog-mode.el --- init program related stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package init-projectile
;;   :defer nil
;;   :load-path "./lisp"
;;   ;; :hook (after-init . 'projectile-mode)
;;   )

(use-package projectile
  ;; https://github.com/jwiegley/use-package#extending-the-load-path
  :bind-keymap ("C-c p" . projectile-command-map)  ;;default is?.
  :init (setq projectile-use-git-grep t)
  :config
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  (setq prj-cache-file
	(expand-file-name ".cache/projectile/projects.cache" user-emacs-directory))
  (message "projects cache file = %s" prj-cache-file)
  (setq
   ;;   projectile-enable-caching t
   ;;   projectile-indexing-method 'alien
   ;;   projectile-require-project-root t
   ;;   projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".Po")
   ;;   ;;projectile-globally-ignored-directories '(".deps")
   projectile-completion-system 'helm ;;经常用ivy https://www.jianshu.com/p/250301ebb7f9
   )
  (append '(".deps" "*.cache" "*.git" "Downloads" "Documents") projectile-globally-ignored-directories)
  (use-package helm-projectile)
  (use-package ibuffer-projectile))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :init
  (projectile-mode t)
  )

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  :load-path "./lisp/rustic"
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
  (setq lsp-signature-auto-activate t)

  ;; comment to disable rustfmt on save ;;need rustfmt.toml edition=2021
  (setq rustic-format-on-save t)
  (setq rustic-kill-buffer-and-window t)
  ;; :hook (before-save . delete-trailing-whitespace)
  :hook (rustic-mode . rk/rustic-mode-hook))

;; https://github.com/brotzeit/rustic/issues/253
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (if buffer-file-name
      (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :commands lsp
  :bind (:map lsp-mode-map
	      ([remap xref-find-apropos] . helm-lsp-workspace-symbol))
  ;; :when (derived-mode-p 'rust-mode)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (lsp-treemacs-sync-mode 1)
  ;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :hook ((lsp-mode . lsp-ui-mode)
	 ;; (lsp-mode . lsp-enable-which-key-integration)
	 ((c-mode c++-mode) . lsp)
	 ;; ((c-mode c++-mode) . tab2space)
	 ((c-mode c++-mode) . my-c-mode-common-hook )
	 )
  )

(defun my-c-mode-common-hook()
  ;; (c-set-style "stroustrup")
  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
  ;;; hungry-delete and auto-newline
  ;; (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(C-f7)] 'compile)
  ;; (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; (add-hook 'write-contents-functions 'cleanup-buffer-notabs nil t)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.phtml\\'" ))

(use-package js2-mode
  :hook (js-mode . tab2space)
  )

(defun tab2space()
  (interactive)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq tab-width 4
	js-indent-level  4
	indent-tabs-mode nil))

(use-package clang-format :bind
  ("C-c C-r" . clang-format-region))

;; (use-package astyle
;;   :load-path "./lisp/astyle"
;;   :when (executable-find "astyle")
;;   :init (use-package reformatter)
;;   :hook (c-mode-common . astyle-on-save-mode))
(use-package format-all
  :demand
  :load-path "./lisp/format-all-the-code")

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

;; (use-package dap-cpptools
;;   ;; :load-path "./lisp/dap-mode"
;;   )
;; (use-package dap-ui
;;   ;; :load-path "./lisp/dap-mode"
;;   )

(use-package dap-mode
  ;; :load-path "./lisp/dap-mode"
  :config
  (dap-ui-mode)
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

;; (use-package init-cc-mode
;;   :load-path "./lisp")

(provide 'init-prog-mode)
;;; init-prog-mode.el ends here
