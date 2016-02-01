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
                 '("~/.emacs.d/lisp"
				   "~/.wl"
				   "~/.folders")
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
(setenv "LANG" "ja_JP.UTF-8")

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
(global-set-key (kbd "C-c C-p") 'json-pretty-print-buffer)		;json-pritty-print
(global-set-key (kbd "C-c C-u") 'unicode-unescape-region)		;unicode-unescape-region
(global-set-key (kbd "C-x C-c") 'nil)							;終了コマンドを無効
(global-set-key (kbd "s-C") 'nil)								;Colorsポップアップを無効

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
  (set-frame-parameter nil 'alpha 80);←透過具合
))

;; Ujelly-beans(color-scheme)
(if window-system
     (when (>= emacs-major-version 24)
       (when (require 'ujelly-theme)
         (load-theme 'ujelly t))))
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

;;============================
;; dired.conf
;;============================
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
;; diredのファイルサイズ表記を"MB,KB"のように変更
(setq dired-listing-switches "-alh")
;; diredのソートを便利に。
(defvar dired-various-sort-type
  '(("S" . "size")
    ("X" . "extension")
    ("v" . "version")
    ("t" . "date")
    (""  . "name")))

(defun dired-various-sort-change (sort-type-alist &optional prior-pair)
  (when (eq major-mode 'dired-mode)
    (let* (case-fold-search
           get-next
           (options
            (mapconcat 'car sort-type-alist ""))
           (opt-desc-pair
            (or prior-pair
                (catch 'found
                  (dolist (pair sort-type-alist)
                    (when get-next
                      (throw 'found pair))
                    (setq get-next (string-match (car pair) dired-actual-switches)))
                  (car sort-type-alist)))))
      (setq dired-actual-switches
            (concat "-l" (dired-replace-in-string (concat "[l" options "-]")
                                                  ""
                                                  dired-actual-switches)
                    (car opt-desc-pair)))
      (setq mode-name
            (concat "Dired by " (cdr opt-desc-pair)))
      (force-mode-line-update)
      (revert-buffer))))

(defun dired-various-sort-change-or-edit (&optional arg)
  "Hehe"
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-various-sort-change dired-various-sort-type)))

(defvar anything-c-source-dired-various-sort
  '((name . "Dired various sort type")
    (candidates . (lambda ()
                    (mapcar (lambda (x)
                              (cons (concat (cdr x) " (" (car x) ")") x))
                            dired-various-sort-type)))
    (action . (("Set sort type" . (lambda (candidate)
                                    (dired-various-sort-change dired-various-sort-type candidate)))))
    ))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" 'dired-various-sort-change-or-edit)
             (define-key dired-mode-map "c"
               '(lambda ()
                  (interactive)
                  (anything '(anything-c-source-dired-various-sort))))
             ))

(defun dired-dwim-find-alternate-file ()
  "画面分割に適した `dired-find-alternate-file'．
通常は `dired-find-alternate-file' を行うが，画面分割されていて
他方のウィンドウに同じバッファが表示されていれば `dired-find-file'．"
  (interactive)
  (cond
   ;; 同じバッファが他のwindowにある場合
   ((delq (selected-window) (get-buffer-window-list))
    (dired-find-file))
   ;; 同じバッファが他のwindowにない場合
   (t
    (dired-find-alternate-file))))

(defun dired-up-alternate-directory ()
  "バッファを増やさず上のディレクトリに移動．"
  (interactive)
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (find-alternate-file up)
          (dired-goto-file dir)))))

(defun dired-dwim-up-alternate-directory ()
  "画面分割に適した `dired-up-alternate-directory'．"
  (interactive)
  (cond
   ;; 同じバッファが他のwindowにある場合
   ((delq (selected-window) (get-buffer-window-list))
    (dired-up-directory))
   ;; 同じバッファが他のwindowにない場合
   (t
    (dired-up-alternate-directory))))

(defun dired-dwim-quit-window ()
  "画面分割に適した `quit-window'．"
  (interactive)
  (quit-window (not (delq (selected-window) (get-buffer-window-list)))))

;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-dwim-find-alternate-file)
(define-key dired-mode-map (kbd "C-m") 'dired-dwim-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;;============================
;; /dired.conf
;;============================

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
(setq php-mode-force-pear nil)
;(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook
          (lambda ()
			(subword-mode t)
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
(require 'ac-helm)
(require 'helm-descbinds)
(require 'helm-ls-git)
(require 'helm-gtags)
(helm-mode 1)

(define-key global-map (kbd "C-;")  	'helm-mini)
(define-key global-map (kbd "M-x")    	'helm-M-x)
(define-key global-map (kbd "C-x C-f")	'helm-find-files)
(define-key global-map (kbd "C-x C-r")	'helm-recentf)
(define-key global-map (kbd "C-c o")	'helm-occur)
(define-key global-map (kbd "C-c a")	'helm-do-ag)
(define-key global-map (kbd "M-y")    	'helm-show-kill-ring)
;(define-key global-map (kbd "C-x b")  	'helm-buffers-list)
(define-key global-map (kbd "C-c b")  	'helm-descbinds)
(define-key global-map (kbd "C-M-z")  	'helm-resume)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "C-z") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-z") 'helm-execute-persistent-action)

;; helm-gtags
(add-hook 'helm-gtags-mode-hook
		  '(lambda ()
			 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)		;;入力されたタグの定義元へジャンプ
			 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)		;;入力タグを参照する場所へジャンプ
			 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)	;;入力したシンボルを参照する場所へジャンプ
			 (local-set-key (kbd "M-l") 'helm-gtags-select)		;;タグ一覧からタグを選択し, その定義元にジャンプする
			 (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)		;;ジャンプ前の場所に戻る
			 ))
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'web-mode-hook 'helm-gtags-mode)
(add-hook 'js-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode-hook 'helm-gtags-mode)
(add-hook 'jsx-mode-hook 'helm-gtags-mode)

;; ctags
(require 'ctags nil t)
(setq tags-revert-without-query t)
(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
(global-set-key (kbd "M-.") 'ctags-search)

;; ac-helm
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;; helm-ag
(setq helm-ag-base-command "ag --nocolor --nogroup -S")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-mini-default-sources
   (quote
	(helm-source-buffers-list helm-source-ls-git helm-source-files-in-current-dir helm-source-recentf)))
 '(helm-truncate-lines t t)
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(org-agenda-files
   (quote
	("~/Dropbox/org/memo.org" "/Users/akihiro_uchida/Dropbox/org/project/daily.org" "/Users/akihiro_uchida/Dropbox/org/project/project.org"))))
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

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

;;flycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'jsx-mode-hook 'flycheck-mode)
;(add-hook 'web-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-jsx-mode-hook 'flycheck-mode)

;;web-mode
;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
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

;; undo-hist
(require 'undohist)


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
;(set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
;(set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
;(set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化
;; 72 文字折り返しをオミット
(add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

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

;; リモートのファイルは確認しない
(setq recentf-exclude '("^/[^/:]+:"))

;;vagrant-tramp
(eval-after-load 'tramp
  '(vagrant-tramp-enable))

;;auto-highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;;point-undo
(require 'point-undo)
;(define-key global-map (kbd "C-u") 'point-undo)
;(define-key global-map (kbd "C-M-u") 'point-redo)

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

;; ==================
;; Eshell
;; ==================
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

;; eshell/less
;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
"Invoke `view-file’ on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
(interactive)
(while args
(if (string-match "\\`\\+\\([0-9]+\\)\\’" (car args))
(let* ((line (string-to-number (match-string 1 (pop args))))
(file (pop args)))
(view-file file)
(goto-line line))
(view-file (pop args)))))


;;popwin.el
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:adjust-other-windows t)
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:special-display-config '(("*compilatoin*" :noselect t)
                                        ("helm" :regexp t :height 0.4)
										("*magit:" :regexp t :height 0.9)
										("COMMIT_EDITMSG" :height 0.8)
										("*magit-" :noselect :height 0.4)
										("*HTTP Response*" :height 0.6)
                                        )))
 ;; disable popwin-mode in an active Helm session It should be disabled
 ;; otherwise it will conflict with other window opened by Helm persistent
 ;; action, such as *Help* window.
(push '("^\*helm.+\*$" :regexp t :height 0.5 :position :bottom) popwin:special-display-config)
(add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))

 ;;  Restore popwin-mode after a Helm session finishes.
 (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))


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

;; slime
(setq inferior-lisp-program "/usr/local/bin/clisp")
(add-to-list 'load-path "/Applications/slime")
(require 'slime)
(slime-setup)

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
;;(global-key-combo-mode 1)
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

;; rainbow-mode.el
(require 'rainbow-mode)
 (defun my-rainbow-mode-turn-on ()
   (rainbow-mode t))
 (add-hook 'css-mode-hook 'my-rainbow-mode-turn-on)
 (add-hook 'scss-mode-hook 'my-rainbow-mode-turn-on)
 (add-hook 'html-mode-hook 'my-rainbow-mode-turn-on)

;; ===============================
;; evil
;; ===============================
;; 挿入ステートを抜けるときにカーソルを後退する
(setq evil-move-cursor-back nil)

(defadvice evil-paste-pop (around evil-paste-or-move-line activate)
  ;; evil-paste-popできなかったらprevious-lineする
  "If there is no just-yanked stretch of killed text, just move
to previous line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop)
               (call-interactively 'previous-line)
             (signal (car err) (cdr err))))))
(defadvice evil-paste-pop-next (around evil-paste-or-move-line activate)
  ;; evil-paste-pop-nextできなかったらnext-lineする
  "If there is no just-yanked stretch of killed text, just move
to next line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop-next)
               (call-interactively 'next-line)
             (signal (car err) (cdr err))))))

(require 'evil)
(require 'evil-org)
(require 'evil-leader)
(global-evil-leader-mode)                 ; (evil-mode t)より先に設定する必要が有る
(evil-leader/set-leader ",")              ; <Leader> を , に設定
;; markdown-mode で ,mbp にバインド
;(evil-leader/set-key-for-mode 'markdown-mode "mbp" 'my-blog-post)
(evil-mode t)
(setcdr evil-insert-state-map nil)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "M-+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "M-_") 'evil-numbers/dec-at-pt)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-t") nil)
(evil-leader/set-key "a" 'org-agenda)

;(define-key evil-emacs-state-map [escape] 'evil-normal-state)
;(define-key evil-emacs-state-map (kbd "C-[") 'evil-normal-state)

;; for magit
(eval-after-load 'evil-core
  '(evil-set-initial-state 'magit-popup-mode 'emacs))

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;(setq evil-default-cursor 'hbar
;      evil-normal-state-cursor '("darkolivegreen")      ; ノーマルステートでは穏かな緑の水平バーに.
;      evil-insert-state-cursor '("#800000" (bar . 2)))  ; 挿入ステートでは目立つ赤い垂直バーに.
;(blink-cursor-mode -1)

;; powerline.el
(require 'powerline)
(require 'powerline-evil)
(powerline-center-evil-theme)
(setq ns-use-srgb-colorspace nil)
(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#FF0066"
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :foreground "#fff"
                    :background "#FF6699"
                    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
                    :foreground "#000"
                    :background "#ffaeb9"
                    :inherit 'mode-line)
; マイナーモード名を隠す
(setq my/hidden-minor-modes
      '(undo-tree-mode
        eldoc-mode
		git-gutter+-mode
		helm-gtags-mode
		yas-minor-mode
        auto-complete-mode
        magit-auto-revert-mode
        abbrev-mode
        helm-mode))

(mapc (lambda (mode)
          (setq minor-mode-alist
                (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
        my/hidden-minor-modes)


;; markdown-preview
(require 'w3m)
(require 'markdown-mode)

(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))

(defun markdown-render-w3m (n)
  (interactive "p")
  (message (buffer-file-name))
  (call-process "/usr/local/bin/grip" nil nil nil
                "--gfm" "--export"
                (buffer-file-name)
                "/tmp/grip.html")
  (w3m-browse-url-other-window "file://tmp/grip.html")
  )
(define-key markdown-mode-map "\C-c \C-c" 'markdown-render-w3m)

;; dired evil-mode
(require 'f)
(evil-make-overriding-map dired-mode-map 'normal)
(evil-define-key 'normal dired-mode-map
  ";" (lookup-key evil-motion-state-map ";")
  "j" 'dired-next-line                        ; 人差し指
  "k" 'dired-previous-line                    ; 中指
  "h" 'dired-up-alternate-directory           ; 人差し指の左
;  "l" 'keu-dired-down-directory               ; 薬指
  "l" 'dired-dwim-find-alternate-file        ; 薬指
  "m" (lookup-key evil-normal-state-map "m")
  "w" (lookup-key evil-normal-state-map "w")
  (kbd "SPC")   (lookup-key dired-mode-map "m")
  (kbd "S-SPC") (lookup-key dired-mode-map "d")
  )

(defun keu-dired-down-directory ()
  "[Dired command] Go down to the directory."
  (interactive)
  (condition-case err
      (let path (dired-get-file-for-visit)
        (if (f-directory? path)
            (dired-dwim-find-alternate-file)
            (message "This is not directory!")))
    (error (message "%s" (cadr err)))))

;; dash-at-point
;(global-set-key "\C-cd" 'dash-at-point)
;(global-set-key "\C-ce" 'dash-at-point-with-docset)

(require 'json-reformat)
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.rc$'" . restclient-mode))


;; #+:Emacs
(defun unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

(defun unicode-escape-region (&optional start end)
  "指定した範囲の文字をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (format "\\u%04x"
                             (char-unicode
                              (char (match-string 0) 0)))
                     nil t))))

;; #+:Emacs
;; こちらも参照→ http://github.com/kosh04/emacs-lisp > xyzzy.el
(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

;; ace-jump-mode
(require 'ace-jump-mode)
;;If you use evil
(define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-char-mode)

;; term+ config
(require 'term+)
(require 'term+mux)
;(require 'xterm-color)
;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
      ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))
;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq multi-term-program "/usr/local/bin/zsh")
(setenv "TERMINFO" "~/.terminfo")

;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)

;; Twitter
(require 'twittering-mode);

;; Wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl" "Write draft with Wanderlust." t)

;; set eshell aliases
(setq eshell-command-aliases-list
      (append
	   (list
		(list "f" "find-file")
		(list "desk" "cd ~/Desktop")
		(list "swipl" "/opt/local/bin/swipl")
		(list "cd_htdocs" "cd /Applications/MAMP/htdocs") ;#ここのパスは適宜
		(list "cd_frekul" "cd_htdocs;cd frekul.vpn")
		(list "cd_lumit" "cd_htdocs;cd lumit")
		(list "cd_tadaoto" "cd_htdocs;cd tadaoto")
		(list "cd_livon" "cd_htdocs;cd livon")
		(list "cd_webtoru" "cd_htdocs;cd webtoru")
		(list "cd_ws" "cd ~/MyDocuments/WorldScape")
		(list "ssh_l" "ssh lumit")
		(list "ssh_f" "ssh frekul")
		(list "sbash" "source ~/.bashrc")
		(list "livon_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_frekul livon_release")
		(list "lumit_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_lumit l_release")
		(list "dlumit_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_lumit dl_release")
		(list "dlumit_docker_run" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_lumit dl_docker_run")
		(list "af_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_frekul af_release")
		(list "f_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_frekul f_release")
		(list "dvj_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_dvj dvj_release")
		(list "dtada_rel" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_tadaoto dev_release")
		(list "dtada_mi" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_tadaoto multiple_import")
		(list "cakeshell" "/Applications/MAMP/bin/php/php5.3.29/bin/php /Applications/MAMP/htdocs/frekul.vpn/cake/console/cake.php")
		(list "rm_cache" "fab -f /Users/akihiro_uchida/MyDocuments/myscript/fabric/fab_frekul rm_cache"))
	   eshell-command-aliases-list))


;;========================
;; org-mode
;;========================
(require 'ox-gfm)
(setq org-src-fontify-natively t)
(setq org-directory "~/Dropbox/org/project")
(setq org-agenda-files (list org-directory))
(add-hook 'org-mode-hook
		  '(lambda ()
			 (evil-leader/set-key "m" 'org-capture)    ; ,m にバインド
			 (evil-leader/set-key "x" 'org-toggle-checkbox)
			 (evil-leader/set-key "e" 'org-set-effort)
			 (evil-leader/set-key "n" 'org-add-note)
			 (define-key org-mode-map (kbd "C-c C-v") 'revert-buffer)
			 (define-key org-mode-map (kbd "C-c C-i") 'org-mark-ring-goto)
			 (define-key org-mode-map [(control tab)] nil) ;; C-tab はウインドウの移動に使いたいのではずす
			 ))

;; capture templates
(setq org-capture-templates
      '(("p" "Project Task" entry (file+headline (expand-file-name "~/Dropbox/org/project/project.org") "Inbox")
             "** TODO %?\n    %i\n    %a\n    %T")
        ("m" "memo" entry (file (expand-file-name "~/Dropbox/org/memo.org"))
             "* %?\n    %i\n    %a\n    %T")))

; global Effort estimate values
(setq org-global-properties (quote ((
      "Effort_ALL" . "00:10 00:15 00:30 00:45 01:00 01:30 02:00 02:30 03:00"))))
;; カラムビューで表示する項目
;; Column の書式は以下.
;; [http://orgmode.org/manual/Column-attributes.html#Column-attributes
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM_T(Clock)")
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-agenda-custom-commands
      '(
       ("n" "Next Action List"
        tags-todo "next"
        ((org-agenda-prefix-format " %6e ")))))
(setq org-agenda-custom-commands
      '(
       ("c" ""
        tags-todo "SCHEDULED=\"<+0d>\""
        ((org-agenda-overriding-header "TaskChute TODO")
         (org-agenda-overriding-columns-format "%50ITEM(Task) %10Effort(Effort){:}")
         (org-agenda-view-columns-initially t)))))


;; projectile
(require 'projectile)

;; SQL Mode
(require 'sql)
(setq sql-connection-alist
      '((server1 (sql-product 'mysql)
                  (sql-port 3306)
                  (sql-server "localhost")
                  (sql-user "root")
                  (sql-password "root")
                  (sql-database "test_prod_frekul"))
		(server (sql-product 'mysql)
                  (sql-port 3306)
                  (sql-server "localhost")
                  (sql-user "root")
                  (sql-password "root")
                  (sql-database "tadaoto"))))

(defun my-sql-server1 ()
  (interactive)
  (my-sql-connect 'mysql'server1))

(defun my-sql-server2 ()
  (interactive)
  (my-sql-connect 'mysql 'server2))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq exec-path (cons "/Applications/MAMP/Library/bin/" exec-path))

;; スタートアップ非表示
(setq inhibit-startup-message t)
;; スクロールバー非表示
(set-scroll-bar-mode nil)
;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))
