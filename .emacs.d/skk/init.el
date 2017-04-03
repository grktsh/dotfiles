;; -*- mode: emacs-lisp; coding: utf-8-unix -*-

;; @@ 基本中の基本の設定

;; 辞書サーバを使うとき
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)
(setq skk-server-report-response t)

;; SHIFT キーを使うことなく、見出し語・送り仮名の位置を指定する
(setq skk-sticky-key ";")

;; モードライン上の表示を変更する
;;   'left => モードラインの左端に表示する
;;   以外  => マイナーモードとして表示する
(setq skk-status-indicator 'minor-mode)

;; 変換時に注釈 (annotation) を表示する
(setq skk-show-annotation t)

;; 注釈を表示するまでの遅延を秒で指定する
(setq skk-annotation-delay 0)

;; 変換前/変換中にエコーエリアに冗長なメッセージを表示
(setq skk-verbose t)

;; 変換候補一覧と注釈 (annotation) の表示を
;; エコーエリアに代えて tooltip で表示する
;; 注) 今のところ FSF Emacs 21 以上と XEmacs 21.5 以上で機能します。
(setq skk-show-tooltip t)

;; Tip 描画に popup-tip を利用してみる
(when (require 'popup nil t)
  (setq skk-tooltip-function 'popup-tip)
  (setq skk-annotation-function
	(lambda (annotation)
	  (popup-tip
	   (skk-eval-string (skk-annotation-get annotation)))
	  nil)))

;; @@ 基本的なユーザ・インターフェース

;; ▼モードで Enter キーを押したとき
;;   nil => 確定と改行（デフォルト）
;;   non-nil => 確定するのみ。改行しない。
(setq skk-egg-like-newline t)

;; 対応する閉括弧を自動的に挿入する
(setq skk-auto-insert-paren t)

;; リージョンを括弧で囲む
(setq skk-use-auto-enclose-pair-of-region t)

;; ;; 動的な補完を使う
;; (setq skk-dcomp-activate t)

;; ;; 動的補完で候補を複数表示する （XEmacs では機能しません）
;; (setq skk-dcomp-multiple-activate t)
;; (setq skk-dcomp-multiple-rows 10)

;; @@ 変換動作の調整

;; 送り仮名が厳密に正しい候補を優先して表示する
(setq skk-henkan-strict-okuri-precedence t)

;; 辞書登録のとき、余計な送り仮名を送らないようにする
(setq skk-check-okurigana-on-touroku 'auto)

;; 変換の学習
(require 'skk-study)

;; @@ 検索に関連した設定

;; look コマンドを使った検索を行う
(setq skk-use-look t)


;; 数値変換機能を使う
(setq skk-use-numeric-conversion t)

;; @@ かな入力関連の設定

;; 半角カナ入力メソッドを使う
(setq skk-use-jisx0201-input-method t)

;; @@ 個人辞書に関する設定

;; 複数の Emacsen を起動して個人辞書を共有する
(setq skk-share-private-jisyo t)

;; 10 分放置すると個人辞書が自動的に保存される設定
(defvar skk-auto-save-jisyo-interval 600)
(defun skk-auto-save-jisyo ()
  (skk-save-jisyo)
  ;; ;; skk-bayesian.el を使っていなければ以下の 2 行はコメントアウト
  ;; (when (require 'skk-bayesian nil t)
  ;;   (skk-bayesian-save-history)
  ;;   (skk-bayesian-corpus-save))
  )
(run-with-idle-timer skk-auto-save-jisyo-interval
		     skk-auto-save-jisyo-interval
		     'skk-auto-save-jisyo)
;; (cancel-function-timers 'skk-auto-save-jisyo)

;; 個人辞書の文字コードを指定する
(setq skk-jisyo-code 'utf-8)

;; @@ server completion
;; (add-to-list 'skk-search-prog-list
;;	     '(skk-server-completion-search) t)
;; (add-to-list 'skk-completion-prog-list
;;	     '(skk-comp-by-server-completion) t)

;; @@ その他いろいろ

;; かなモードで、モード変更を行なわずに
;;   長音(ー)を ASCII 数字の直後では `-' に、全角数字の直後では `−' に
;;   句点(。)を ASCII 数字の直後では `.' に、全角数字の直後では `．' に
;;   読点(、)を ASCII 数字の直後では `,' に、全角数字の直後では `，' に
(setq skk-rom-kana-rule-list
      (append '(("-" nil skk-hyphen)
		;; ("." nil skk-hyphen)
		;; ("," nil skk-hyphen)
		)
	      skk-rom-kana-rule-list))

(defun skk-hyphen (arg)
  (let ((output (cdr (assq (skk-last-command-char)
			   '((?- . ("ー" "-" "−" "ー"))
			     (?, . ("、" "," "，" "、"))
			     (?. . ("。" "." "．" "。"))))))
	(c (char-before (point))))
    (cond ((null c) (nth 0 output))
	  ((and (<= ?0 c) (>= ?9 c)) (nth 1 output))
	  ((and (<= ?０ c) (>= ?９ c)) (nth 2 output))
	  (t (nth 3 output)))))

;; Tips といえるものではないが、`lisp-interaction-mode' において "C-j"
;; (`eval-print-last-sexp') を利用する人にとっては、英数モードにおいて
;; "C-j" によって かなモードに入る仕様は使いづらい。
(defadvice skk-latin-mode (after no-latin-mode-in-lisp-interaction activate)
  "`lisp-interaction-mode' において英数モードを回避する。"
  (if (eq major-mode 'lisp-interaction-mode)
    (skk-mode-off)))

(setq skk-rom-kana-rule-list
      (append '(("z " nil "　")
		("z(" nil "《")
		("(" nil "（")
		(")" nil "）")
		("!" nil "！"))
	      skk-rom-kana-rule-list))
