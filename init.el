;; ロードパス
(setq load-path (append
                 '("~/.emacs.d")
                 load-path))

;;package
(require 'package);
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; 日本語環境
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/
;; 英語
(set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 120)    ;; font size
;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.1)));; Mac用フォント設定

;;yalinum
(toggle-scroll-bar nil) ;スクロール非表示
(global-yalinum-mode t)
(set-face-background 'yalinum-bar-face "DarkOliveGreen")

;; 基本キーバインド
(define-key global-map (kbd "C-h") 'delete-backward-char)	; 削除
(define-key global-map (kbd "M-?") 'help-for-help)			; ヘルプ
(define-key global-map (kbd "C-z") 'undo)					; undo
(define-key global-map (kbd "C-c i") 'indent-region)		; インデント
(define-key global-map (kbd "C-c M-a") 'align-regexp)		;対象文字でインデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)		; 補完
;(define-key global-map (kbd "C-c g") 'moccur)				;grep
(define-key global-map (kbd "C-c ;") 'comment-dwim)		; コメントアウト
(define-key global-map (kbd "\C-m") 'newline-and-indent)	; インデント(改行)


;; C-i でタブを入力できるように
(global-set-key "\C-i" '(lambda ()
  (interactive)
  (insert "\t")))

;; ウィンドウサイズの位置、サイズ
;;(set-frame-parameter nil 'fullscreen 'maximized) ;;最大化
(if window-system (progn
  (setq initial-frame-alist '((width . 179)(height . 53)(top . 0)(left . 0)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Yellow")
  (set-frame-parameter nil 'alpha 83);←透過具合
))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; 選択中のリージョンの色を設定します。
(set-face-background 'region "deeppink1")

;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; カーソルの点滅をとめる
;;(blink-cursor-mode 0)

;; "yes or no"を"y or n"に
(fset 'yes-or-no-p 'y-or-n-p)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; 対応する括弧の色の設定
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")


;; 空白文字の表示
(require 'whitespace)
(global-whitespace-mode 1)

(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSlateGray"
                    :underline nil)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)


;; C-x C-b でバッファリストを開く時に、ウィンドウを分割しない
(global-set-key "\C-x\C-b" 'buffer-menu)

;; ウィンドウ内に収まらないときだけカッコ内も光らせる
(setq show-paren-style 'mixed)

;; 行末の空白を表示
;(setq-default show-trailing-whitespace t)

;; 現在行を目立たせる
;;(global-hl-line-mode)

;; 行の先頭をC-kを一回押すだけで行全体を表示する
(setq kill-whole-line t)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 補完
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完可能なものを随時表示
(icomplete-mode 1)

;; 履歴数
(setq history-length 10000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; 最近開いたファイルを保存する数
(setq recentf-max-saved-items 10000)

;;複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)
;; diffのオプション
(setq diff-switches '("-u" "-p" "-N"))

;; 1行づつスクロールする
(setq scroll-conservatively 1)

;; diredを便利にする
(require 'dired-x)

;; diredから"r"でファイル名インライン編集する
(require 'wdired)
(ffap-bindings)
(add-hook 'dired-mode-hook (lambda ()
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;;FTP接続
;;C-x d /username@hostname:/directory/

;; ange-ftp
(require 'ange-ftp)
;(setq ange-ftp-default-user "afterlineuchida@sotsuken.rakusaba.jp")
;(ange-ftp-set-passwd "sotsuken.rakusaba.jp" "afterlineuchida@sotsuken.rakusaba.jp" "closeupmagic")
(setq ange-ftp-try-passive-mode t)


;; デフォルトのタブ
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
;;(setq indent-line-function 'indent-relative-maybe)

;;タブ幅の倍数を設定
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))


;;; init-loader
;(setq load-path
;      (append
;       (list
;        (expand-file-name "~/.emacs.d/elpa/")
;        )
;       load-path))
(require 'init-loader)
(init-loader-load "~/.emacs.d/config")


;; php-mode
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$'" . php-mode))
(setq php-mode-force-pear t)

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elpa/")
(add-to-list 'load-path auto-install-directory)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;smartparens.el
;;括弧の自動補完
(require 'smartparens)
(smartparens-global-mode t)


;;dupulicate-thing
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

;; Objective-C mode
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
(add-hook 'c-mode-common-hook
		  '(lambda()
             (c-set-style "cc-mode")))

;;color-moccur.el
(require 'color-moccur)
(setq moccur-split-word t) ;スペースで区切られた複数の単語にマッチさせる

;;moccur-edit.el
(require 'moccur-edit)

;;helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)

(define-key global-map (kbd "M-x")    	'helm-M-x)
(define-key global-map (kbd "C-x C-f")	'helm-find-files)
(define-key global-map (kbd "C-x C-r")	'helm-recentf)
(define-key global-map (kbd "C-c o")	'helm-occur)
(define-key global-map (kbd "C-c g")	'helm-ag)
(define-key global-map (kbd "M-y")    	'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")  	'helm-imenu)
;(define-key global-map (kbd "C-x b")  	'helm-buffers-list)
(define-key global-map (kbd "C-x b")  	'helm-mini)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)


;;flycheck
(require 'flycheck)
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
;(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;(add-hook 'web-mode-hook 'flycheck-mode)
;(add-hook 'js-mode-hook 'flycheck-mode)

;; ファイル名が重複していたらディレクトリ名を追加する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;undo-tree.el
(require 'undo-tree)
(global-undo-tree-mode t)


;;multiple-cursors etc
(require 'expand-region)
(require 'multiple-cursors)
(require 'smartrep)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
 global-map "C-'" '(("n" . 'mc/mark-next-like-this)
					("p" . 'mc/mark-previous-like-this)
					("u" . 'mc/unmark-next-like-this)
					("U" . 'mc/unmark-previous-like-this)
					("s" . 'mc/skip-to-next-like-this)
					("S" . 'mc/skip-to-previous-like-this)
					("i" . 'my/mc/insert-numbers) 
					("*"   . 'mc/mark-all-like-this)))

;; insert specific serial number
    (defvar my/mc/insert-numbers-hist nil)
    (defvar my/mc/insert-numbers-inc 1)
    (defvar my/mc/insert-numbers-pad "%01d")

    (defun my/mc/insert-numbers (start inc pad)
      "Insert increasing numbers for each cursor specifically."
      (interactive
       (list (read-number "Start from: " 0)
             (read-number "Increment by: " 1)
             (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
      (setq mc--insert-numbers-number start)
      (setq my/mc/insert-numbers-inc inc)
      (setq my/mc/insert-numbers-pad pad)
      (mc/for-each-cursor-ordered
       (mc/execute-command-for-fake-cursor
        'my/mc--insert-number-and-increase
        cursor)))

    (defun my/mc--insert-number-and-increase ()
      (interactive)
      (insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
      (setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))

;; load environment value
;(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
;(dolist (path (reverse (split-string (getenv "PATH") ":")))
;  (add-to-list 'exec-path path))

;;magit.el
(require 'magit)

;;git-gutter+
(require 'git-gutter+)
(global-git-gutter+-mode t)

;;migemo
(require 'migemo)

;; スタートアップ非表示
(setq inhibit-startup-message t)

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)
