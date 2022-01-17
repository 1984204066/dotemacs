;;; init-org-git.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; (require 'init-org-roam)
;; https://askubuntu.com/questions/348999/how-to-use-the-latest-stable-version-of-org-mode
;; (add-to-list 'load-path "~/src/org-mode/lisp")
;; (require 'org-loaddefs)
(use-package org
  ;; :demand
  :mode (("\\.org$" . org-mode))
  :load-path "./lisp/org-mode/lisp"
  :init
  (require 'org-loaddefs)
  :bind (:map org-mode-map
	      ("C-S-j" . org-return-maybe-indent)
	      ("C-j" . join-line))
  :hook (org-mode . (lambda() (setq truncate-lines nil)))
  ;; (global-visual-line-mode 1)
  :config
  ;; (setq org-log-done 'time)
  (require 'ox-md nil t)
  (setq org-export-with-toc nil)
  ;; (require 'ob-ditaa nil t)
  ;; active Org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (ditaa . t)
     (shell . t)))
  (setq org-ditaa-jar-path
	(expand-file-name "~/.local/bin/ditaa0_9.jar"))
  )

(use-package markdown-toc
  :hook (markdown-mode . markdown-toc-mode))

(provide 'init-org-git)
;;; init-org-git.el ends here
