;;https://zhuanlan.zhihu.com/p/341512250
(require 'package)
(setq package-enable-at-startup nil)
;; If called as part of loading ‘user-init-file’, set
;; ‘package-enable-at-startup’ to nil, to prevent accidentally
;; loading packages twice.
(setq package-check-signature nil) ;个别时候会出现签名校验失败
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
					;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
					;                         ("marmalade" . "http://marmalade-repo.org/packages/")
					;                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; https://emacs.stackexchange.com/questions/57596/getting-started-use-package-question#:~:text=If%20your%20version%20of%20Emacs%20%28M-x%20emacs-version%20RET%29,will%20effectively%20be%20called%20before%20user-init-file%20is%20loaded.
;; Also, since Emacs 27.1 "it is no longer necessary to call 'package-initialize' in your init file". Therefore, you should be able to safely remove (package-initialize) call or use it conditionally, such as:
;; (when (< emacs-major-version 27)
;;   (package-initialize))

;;https://emacs-china.org/t/topic/4088/3
;; 比较确定的是，在使用 package- 系列函数（例如 (package-install 'dash)) 之前必需调用 (package-initialize)。
;; 有package.el的Emacs都不需要手动添加(package-initialize) Emacs启动的时候会自动运行的
(unless package--initialized (package-initialize t)) ;;(package-initialize)
;; https://segmentfault.com/a/1190000039902535
(unless (package-installed-p 'use-package) ;; (when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package)) ;;保证use-package 能用

(require 'use-package)
(require 'use-package-ensure)

(eval-and-compile
  (setq use-package-always-ensure t) ;不用每个包都手动添加:ensure t关键字
  (setq use-package-always-defer t) ;默认都是延迟加载，不用每个包都手动添加:defer t
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
;;通过 benchmark-init 包进行启动耗时的测量。M-x benchmark-init/show-durations-tree 或者 M-x benchmark-init/show-durations-tabulated
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

(use-package emacs
  :init
  ;; 设置系统的编码，避免各处的乱码。
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)

  (setq inhibit-startup-message t ;;不显示GNU emacs启动界面
	scroll-step 1 ;;显示菜单栏
	make-backup-files nil ;;关闭自动备份
	)
  (setq-default cursor-type 'bar) ;;光标设为竖线
  ;;(tool-bar-mode -1) ;;不显示工具栏 工具栏太丑
  ;;(scroll-bar-mode -1) ;;不显示滚动条 太丑
  (electric-indent-mode t) ;;自动缩进
  ;; 不要总是没完没了的问yes or no, 为什么不能用y/n
  ;;(fset 'yes-or-no-p'y-or-n-p)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (use-package init-parens :demand :load-path "./lisp")
  ;;功能稍微好用于NumberedWindows http://nschum.de/src/emacs/window-numbering-mode/
  (use-package window-numbering :defer 1
    :config
    (window-numbering-mode 1)) ;;采用M-i进行窗口导航切换 M-0切换到激活的minibuffer
  (use-package windresize :defer 2
    :bind (
	   ("<C-M-S-left>" . shrink-window-horizontally)
	   ("<C-M-S-right>" . enlarge-window-horizontally)
	   ("<C-M-S-down>" . shrink-window)
	   ("<C-M-S-up>" . enlarge-window)))

  ;; replace global-set-key/global-unset-key
  :bind (
	 ;;很干脆的杀buffer,不想每次回答个 yes or no 之类的a
	 ;;(global-set-key (kbd "M-k") #'(lambda () (interactive) (kill-buffer (current-buffer)))) ;; #'不要也可以。
	 ("M-k" . (lambda () (interactive) (kill-buffer (current-buffer))))
	 ("M-r" . replace-string)
	 ("M-q" . query-replace)
	 ("M-g" . goto-line)
	 ("M-s" . save-buffer)
	 ("M-v" . yank)
	 ("C-2" . set-mark-command )
	 ("C-j" . join-line)
	 ("C-z" . undo)
	 ([f6] . other-window )
	 ([f7] . delete-other-windows)
	 ("<C-M-left>" . (lambda () (interactive) (insert "←")))
	 ("<C-M-right>" . (lambda () (interactive) (insert "→")))
	 ("M-o" . find-file )
	 ("M-b" . switch-to-buffer)
	 ("C-s" . isearch-forward)
	 ;; ("C-w" kill-region)
	 ;; ([C-f7] . toggle-frame-maximized)
	 ;;(global-set-key (kbd "C-<SPC>") 'forward-char)
	 ("C-<SPC>" . forward-char)
	 )
  ;; :hook (before-save . delete-trailing-whitespace)
  :config
  :unless *is-windows* ; 有些时候我发现在Windows上开启了行号会让屏幕滚动的时候闪烁
  :config
  (setq display-line-numbers-type 'relative) ;行号配置.从Emacs 26开始，自带了行号显示功能
  (global-display-line-numbers-mode t)
  (require 'init-cnfont)
  )
(use-package nhexl-mode)

(require 'init-org-git)

(use-package init-themes
  :load-path "./lisp"
  :hook (after-init . init-themes))

;; 在VSCode或Jetbrains家族的编辑器中，上下移动行/块是非常容易的
(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;; (eval-after-load 'eshell
;;   '(remove-hook 'before-save-hook 'delete-trailing-whitespace) )
;; (eval-after-load 'markdown
;;   '(remove-hook 'before-save-hook 'delete-trailing-whitespace) )

(add-hook 'js-mode-hook (lambda()(interactive)
			  (setq tab-width 4
				default-tab-width 4
				indent-tabs-mode nil)
			  ))
(use-package helm  ;; helm保证要安装上的，否则M-x没法用。
  :bind (
	 ([remap execute-extended-command] . helm-M-x)
	 ([remap find-file]. helm-find-files)
	 ([remap switch-to-buffer] . helm-mini) ;; helm-buffers-list
	 ([M-f7] . maximize-window)
	 ("M-f" . helm-recentf))
  :config
  (use-package helm-lsp :demand)
  (use-package helm-xref :demand)
  )


(use-package which-key
  :defer nil
  :config (which-key-mode))

(use-package init-projectile
  :defer nil
  :load-path "./lisp"
  ;; :hook (after-init . 'projectile-mode)
  )

(use-package web-mode
  :mode ("\\.html?\\'" "\\.phtml\\'" ))
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

;; 建议写入一个单独的文件，这样就会把通过图形界面做的一些配置，写入到这里了。
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
;; https://emacs-china.org/t/custom-file/3108
(when (file-exists-p custom-file)
  (load custom-file))
