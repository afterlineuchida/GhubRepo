;;PATHの引き継ぎ
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; ロードパス
(setq load-path (append
                 '("~/.emacs.d")
                 load-path))

;;package
(require 'package);
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)

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

;; 現在のバッファを消す
(defun my-kill-current-buffer()
  "kill-current-buffer"
  (interactive)
  (kill-buffer (current-buffer)))

;; 基本キーバインド
(global-set-key (kbd "C-h") 'delete-backward-char)				;削除
(global-set-key (kbd "M-?") 'help-for-help)					;ヘルプ
(global-set-key (kbd "C-z") 'undo)								;undo
(global-set-key (kbd "C-c i") 'indent-region)					;インデント
(global-set-key (kbd "C-c M-a") 'align-regexp)					;対象文字でインデント
(global-set-key (kbd "C-c C-i") 'hippie-expand)				;補完
;(global-set-key (kbd "C-c g") 'moccur)						;grep
(global-set-key (kbd "C-c ;") 'comment-dwim)					;コメントアウト
(global-set-key (kbd "C-m") 'newline-and-indent)				;インデント(改行)
(global-set-key (kbd "C-a") 'beginning-of-visual-indented-line) ;行頭へ
(global-set-key (kbd "C-e") 'end-of-visual-line)				;行末へ
(global-set-key (kbd "C-c e") 'eshell)							;eshell
(global-set-key (kbd "C-c l") 'load-file)						;load-file
(global-set-key (kbd "C-M-k") 'my-kill-current-buffer)			;kill-buffer

;; （＝揃えはよく使うのでワンストロークで）
(global-set-key (kbd "C-]")
				'(lambda () (interactive)
				   (setq current-indent-tabs-mode indent-tabs-mode)
				   (setq indent-tabs-mode t)
				   (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=" 1 1 nil)
				   (setq indent-tabs-mode current-indent-tabs-mode)
				   ))

;; タブインデントの時は改行連打でタブだけの行を残す（これは個人的な好み）
;; スペースインデントの時はスペースだけの行は残さない（一般的）
;(global-set-key (kbd "RET") '(lambda () (interactive)
;							   (if (equal indent-tabs-mode t)
;								   (progn (newline) (indent-for-tab-command))
;								 (newline-and-indent)
;								 )))

;; C-aの行頭移動を空白文字とそれ以外とのトグルに。
(defun beginning-of-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun beginning-of-visual-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文 字しかない場合は、行頭に戻る。"
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
        (head-pos (save-excursion (progn (beginning-of-line) (point)))))
    (cond
     ;; 物理行の1行目にいる場合
     ((eq vhead-pos head-pos)
      (if (string-match
           "^[ \t]+$"
           (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ;; 物理行の2行目以降の先頭にいる場合
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     ;; 物理行の2行目以降の途中にいる場合
     (t (beginning-of-visual-line)))))


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
(setq require-final-newline nil)

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
(global-set-key [backspace] 'backward-delete-char)

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
(add-hook 'php-mode-hook
          (lambda ()
            (defun ywb-php-lineup-arglist-intro (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun ywb-php-lineup-arglist-close (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-style "stroustrup")    ; インデントは4文字分基本スタイル
            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro) ; 配列のインデント関係
            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close) ; 配列のインデント関係
            (c-set-offset 'arglist-cont-nonempty' 4) ; 配列のインデント関係
            (c-set-offset 'case-label' 4) ; case はインデントする
            (make-local-variable 'tab-width)
            (make-local-variable 'indent-tabs-mode)
            (setq tab-width 4)
            (setq indent-tabs-mode t)))   ; インデントにタブを使う

;;python-mode
(add-hook 'python-mode-hook
    '(lambda ()
        (setq tab-width 4)
        (setq indent-tabs-mode t)
    ))

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elpa/")
(add-to-list 'load-path auto-install-directory)
;;(auto-install-update-emacswiki-package-name t)
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
(require 'helm-ag)
(require 'helm-descbinds)
(require 'helm-ls-git)
(helm-mode 1)

(define-key global-map (kbd "M-x")    	'helm-M-x)
(define-key global-map (kbd "C-x C-f")	'helm-find-files)
(define-key global-map (kbd "C-x C-r")	'helm-recentf)
(define-key global-map (kbd "C-c o")	'helm-occur)
(define-key global-map (kbd "C-c a")	'helm-ag)
(define-key global-map (kbd "M-y")    	'helm-show-kill-ring)
;(define-key global-map (kbd "C-x b")  	'helm-buffers-list)
(define-key global-map (kbd "C-;")  	'helm-mini)
(define-key global-map (kbd "C-c b")  	'helm-descbinds)
(define-key global-map (kbd "M-.")  	'helm-etags-select)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; helm-ag
(setq helm-ag-base-command "ag --nocolor --nogroup")

(custom-set-variables
   '(helm-truncate-lines t)
   '(helm-delete-minibuffer-contents-from-point t)
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-ls-git
                                 helm-source-files-in-current-dir
                                 helm-source-recentf
                                 )))
;; ag.el
(require 'ag)
(setq ag-highlight-search t)  ; 検索キーワードをハイライト
(setq ag-reuse-buffers t)     ; 検索用バッファを使い回す (検索ごとに新バッファを作らない)

;; wgrep
(add-hook 'ag-mode-hook '(lambda ()
                           (require 'wgrep-ag)
                           (setq wgrep-auto-save-buffer t)  ; 編集完了と同時に保存
                           (setq wgrep-enable-key "r")      ; "r" キーで編集モードに
                           (wgrep-ag-setup)))

;;flycheck
(require 'flycheck)
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
;(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;(add-hook 'web-mode-hook 'flycheck-mode)
;(add-hook 'js-mode-hook 'flycheck-mode)

;;web-mode
;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)

;; ファイル名が重複していたらディレクトリ名を追加する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;undo-tree.el
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)


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

;;; 複数行移動
;; quoted-insertのキーバインドを無効(prefix)化
(global-unset-key (kbd "C-q"))
(smartrep-define-key
    global-map "C-q"
  '(
    ("j" . (next-line 4))		;;4行上移動
    ("k" . (previous-line 4))	;;4行下移動
    ))


;;magit.el
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)
;; 色変更
(set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
(set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
(set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化

;;git-gutter-fringe+.el
(require 'git-gutter+)
(global-git-gutter+-mode +1)
(require 'git-gutter-fringe+)
(git-gutter-fr+-minimal)
(set-face-foreground 'git-gutter-fr+-modified "blue")
(set-face-foreground 'git-gutter-fr+-added    "yellow")
(set-face-foreground 'git-gutter-fr+-deleted  "white")

;;migemo
(require 'migemo)
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)

;;vagrant-tramp
(eval-after-load 'tramp
  '(vagrant-tramp-enable))

;;auto-highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;;point-undo
(require 'point-undo)
(define-key global-map (kbd "C-u") 'point-undo)
(define-key global-map (kbd "C-M-u") 'point-redo)

;; revert-buffer without asking
(defun revert-buffer-force()
  (interactive)
  (revert-buffer nil t)
)
(define-key global-map (kbd "C-c C-v") 'revert-buffer-force)

;;hightlight-symbol
(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1")) ;; 使いたい色を設定、repeatしてくれる

;; 適宜keybindの設定
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

;; Eshell
;; Emacs 起動時に Eshell を起動
;(add-hook 'after-init-hook (lambda () (eshell) ))
;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
;(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-completions nil)
;;補完候補がこの数値以下だとサイクルせずに候補表示
;(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)
;; prompt 文字列の変更
(setq eshell-prompt-function
      (lambda ()
        (concat "AK-MBP: "
                (replace-regexp-in-string "^/.*/" "" (eshell/pwd))
				"$ "
                )))
;; 変更した prompt 文字列に合う形で prompt の初まりを指定 (C-a で"$ "の次にカーソルがくるようにする)
;; これの設定を上手くしとかないとタブ補完も効かなくなるっぽい
(setq eshell-prompt-regexp "^[^#$]*[$#] ")
(add-hook 'eshell-mode-hook '(lambda ()
	(define-key eshell-mode-map (kbd "C-a") 'eshell-bol)))
;; 前のコマンドの履歴取得
(defun eshell-cmdline "M-p"
  (let ((last-command 'eshell-previous-matching-input-from-input))
    (eshell-history-and-bol 'eshell-previous-matching-input-from-input)))
;; 次のコマンドの履歴取得
(defun eshell-cmdline "M-n"
  (let ((last-command 'eshell-previous-matching-input-from-input))
    (eshell-history-and-bol 'eshell-next-input)))
;; 直前の履歴を取得
(defadvice eshell-send-input (after history-position activate)
  (setq-default eshell-history-index -1))
;;; helm で履歴から入力
;(add-hook 'eshell-mode-hook
;          #'(lambda ()
;              (define-key eshell-mode-map
;                (kbd "C-P")
;                'helm-eshell-history)))
;;; helm で補完
;(add-hook 'eshell-mode-hook
;          #'(lambda ()
;              (define-key eshell-mode-map
;                (kbd "C-N")
;                'helm-esh-pcomplete)))

;;rotate.el
(require 'rotate)
(global-set-key (kbd "C-t") 'rotate-layout)
(global-set-key (kbd "M-t") 'rotate-window)

;;cake.el
;(require 'cake)


;;cake2.el
;(require 'cake2)
;(global-cake2 t)
;(cake2-set-default-keymap)

;;emacs-nav
(setq load-path (cons "~/.emacs.d/elpa/emacs-nav-49" load-path))
(require 'nav)
(global-set-key (kbd "C-c n") 'nav)

;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)
;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "C-m") 'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;;; フォルダを開く時, 新しいバッファを作成しない
;; バッファを作成したい時にはoやC-u ^を利用する
;;(defvar my-dired-before-buffer nil)
;;(defadvice dired-advertised-find-file
;;  (before kill-dired-buffer activate)
;;  (setq my-dired-before-buffer (current-buffer)))
;;
;;(defadvice dired-advertised-find-file
;;  (after kill-dired-buffer-after activate)
;;  (if (eq major-mode 'dired-mode)
;;      (kill-buffer my-dired-before-buffer)))
;;
;;(defadvice dired-up-directory
;;  (before kill-up-dired-buffer activate)
;;  (setq my-dired-before-buffer (current-buffer)))
;;
;;(defadvice dired-up-directory
;;  (after kill-up-dired-buffer-after activate)
;;  (if (eq major-mode 'dired-mode)
;;      (kill-buffer my-dired-before-buffer)))

;;popwin.el
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:special-display-config '(("*compilatoin*" :noselect t)
                                        ("helm" :regexp t :height 0.4)
										;("magit" :regexp t :height 0.5)
										("COMMIT_EDITMSG" :height 0.3)
                                        )))

;; emacsでGauche
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "/usr/local/bin/gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (split-window-horizontally (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

(define-key global-map
  "\C-cG" 'scheme-other-window)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "/opt/local/bin/multimarkdown")

;; scss-mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
;; インデント幅を2にする
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (setq tab-width 2)
   )
  )
(add-hook 'scss-mode-hook
  '(lambda() (scss-custom)))

;; key-combo.el
(require 'key-combo)
(global-key-combo-mode 1)
;;; 各モードに対するキー設定
(setq key-combo-lisp-mode-hooks
      '(lisp-mode-hook
        emacs-lisp-mode-hook
        lisp-interaction-mode-hook
        inferior-gauche-mode-hook
        scheme-mode-hook))

(setq key-combo-lisp-default
      '(("."  . " . ")
        (","  . (key-combo-execute-orignal))
        (",@" . " ,@")
        (";"  . (";" ";; " ";"))
        ("="  . ("= " "eq " "equal "))
        (">=" . ">= ")))

(setq key-combo-common-mode-hooks
      '(c-mode-common-hook
        php-mode-hook
        ruby-mode-hook
        cperl-mode-hook
        javascript-mode-hook
        js-mode-hook
        js2-mode-hook))

(setq key-combo-common-default
      '((","  . (", " ","))
        ("="  . (" = " " == " " === " "="))
        ("=>" . " => ")
        ("=~" . " =~ ")
        ("=*" . " =* ")
        ("+"  . (" + " " += " "+"))
        ("+=" . " += ")
        ("-"  . (" - " "->" "-"))
        ("-=" . " -= ")
        ("->" . "->")
        (">"  . (" > " " => " " >= " ">"))
        (">=" . " >= ")
        ("%"  . (" % " " %= " "%"))
        ("%="  . " %= ")
        ("!" . ("!" " != " " !~ "))
        ("!="  . " !== " )
        ("!~" . " !~ ")
        ("~" . (" =~ " "~"))
        ("::" . " :: ")
        ("&"  . (" & " " && " "&"))
        ("&=" . " &= ")
        ("&&=" . " &&= ")
        ("*"  . (" * " "**" "*"))
        ("*="  . " *= " )
        ("<" . (" < " " <= " "<"))
        ("<=" . " <= ")
        ("<<=" . " <<= ")
        ("<-" . " <- ")
        ("|"  . (" ||= " " || " "|"))
        ("|=" . " |= ")
        ("||=" . " ||= ")
        ("/" . ("/`!!'/" "// "))
        ("/=" . " /= ")
        ("/*" . "/* `!!' */")
;;        ("{" . ("{`!!'}" "{"))
;;        ("{|" . "{ |`!!'|  }")
;;        ("\"" . ("\"`!!'\"" "\""))
;;        ("'" . ("'`!!''" "'"))
;;        ("(" . ("(`!!')" "("))
		))


(key-combo-define-hook key-combo-common-mode-hooks
                       'key-combo-common-load-default
                       key-combo-common-default)
(key-combo-define-hook key-combo-lisp-mode-hooks
                       'key-combo-lisp-load-default
                       key-combo-lisp-default)

;; smart-newline.el
(require 'smart-newline)
(add-hook 'php-mode-hook ;; or any major-mode-hooks
  (lambda ()
  (smart-newline-mode t)))
(add-hook 'js-mode-hook
  (lambda ()
  (smart-newline-mode t)))
(add-hook 'web-mode-hook
  (lambda ()
  (smart-newline-mode t)))

;; スタートアップ非表示
(setq inhibit-startup-message t)
