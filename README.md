# My own emacs configure
---

package-archives好像只有一个有用melpa。

> ;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") \
> ;                         ("marmalade" . "http://marmalade-repo.org/packages/") \
> ;                         ("melpa" . "http://melpa.milkbox.net/packages/"))) \
>

## package-initialize

说是不用了，但实际测试还得要。\
;; https://emacs.stackexchange.com/questions/57596/getting-started-use-package-question#:~:text=If%20your%20version%20of%20Emacs%20%28M-x%20emacs-version%20RET%29,will%20effectively%20be%20called%20before%20user-init-file%20is%20loaded. \
;; Also, since Emacs 27.1 "it is no longer necessary to call 'package-initialize' in your init file". Therefore, you should be able to safely remove (package-initialize) call or use it conditionally, such as: \
;; (when (< emacs-major-version 27) \
;;   (package-initialize)) \

;;https://emacs-china.org/t/topic/4088/3

;; 比较确定的是，在使用 package- 系列函数（例如 (package-install 'dash)) 之前必需调用 (package-initialize)。

## 新版emacs可以用.config/emacs/init.el
所以dotemacs以后就不用了。目前它的内容和init.el一样。

## TODO

rust 勉强能用， C/C++ 还没配置好。
