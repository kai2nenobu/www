+++
title = "define-minor-mode で定義されたマイナーモードの挙動"
author = ["Tsunenobu Kai"]
date = 2013-02-10
tags = ["Emacs"]
draft = false
+++

[git-gutter.elが minor-mode、global-minor-modeをサポートしました - Life is very short](http://d.hatena.ne.jp/syohex/20130209/1360393565) を見てて

```emacs-lisp
(global-git-gutter-mode t)
```

でマイナーモードって有効になるんだっけ？ 引数は正の数じゃないと有効にならない
んじゃないっけ？ と思ったのでちょっと調べてみました。

<!--more-->


## 引数による動作の変化 {#引数による動作の変化}

とりあえずマイナーモードの例として `tool-bar-mode` の docstring を見てみると

```text
tool-bar-mode is an interactive compiled Lisp function in `tool-bar.el'.

(tool-bar-mode &optional ARG)

Toggle the tool bar in all graphical frames (Tool Bar mode).
With a prefix argument ARG, enable Tool Bar mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Tool Bar mode if ARG is omitted or nil.
```

"With a prefix argument ARG, enable Tool Bar mode if ARG is positive, and
disable it otherwise." を素直に読むと、引数が `t` だったら無効になるんじゃねー
のと思いました。

よくわからんのでいろんな引数で評価してみると

```emacs-lisp
(tool-bar-mode 1)                   ; => t
(tool-bar-mode 999)                 ; => t
(tool-bar-mode 12.345)              ; => t
(tool-bar-mode 0)                   ; => nil
(tool-bar-mode -1)                  ; => nil
(tool-bar-mode -999)                ; => nil
(tool-bar-mode -12.345)             ; => t
(tool-bar-mode '-)                  ; => nil
(tool-bar-mode '(16))               ; => t
(tool-bar-mode t)                   ; => t
(tool-bar-mode nil)                 ; => t
```

こんな感じになりました。 `(tool-bar-mode -12.345)` が `t` なのが非常にきもいで
すね。どうも非正整数なら無効になるっぽいです。

もっと詳しく調べるために、ソースコードに飛びこみました。最近のマイナーモード
は \`define-minor-mode' というマクロを使って定義されていることが多いです。とい
うわけで easy-mmode.el の中の \`define-minor-mode' の定義を見てみました。

```emacs-lisp
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)
  ((中略)
       (defun ,modefun (&optional arg ,@extra-args)
	 ,(or doc
	      (format (concat "Toggle %s on or off.
With a prefix argument ARG, enable %s if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\｛%s｝") pretty-name pretty-name keymap-sym))
	 ;; Use `toggle' rather than (if ,mode 0 1) so that using
	 ;; repeat-command still does the toggling correctly.
	 (interactive (list (or current-prefix-arg 'toggle)))
	 (let ((,last-message (current-message)))
	   (,@(if setter `(funcall #',setter)
		(list (if (symbolp mode) 'setq 'setf) mode))
	    (if (eq arg 'toggle)
		(not ,mode)
	      ;; A nil argument also means ON now.
	      (> (prefix-numeric-value arg) 0)))
	   ,@body
	   (後略)))))
```

引数 arg の処理は `(prefix-numeric-value arg)` の部分ですね。これもいろんな引
数で評価して見ました。

```emacs-lisp
(prefix-numeric-value 1)                ; => 1
(prefix-numeric-value 999)              ; => 999
(prefix-numeric-value 12.345)           ; => 1
(prefix-numeric-value 0)                ; => 0
(prefix-numeric-value -1)               ; => -1
(prefix-numeric-value -999)             ; => -999
(prefix-numeric-value -12.345)          ; => 1
(prefix-numeric-value '-)               ; => -1
(prefix-numeric-value '(16))            ; => 16
(prefix-numeric-value t)                ; => 1
(prefix-numeric-value nil)              ; => 1
```

なるほど、これが0より大きければマイナーモードが有効になるわけですね。

`prefix-numeric-value` は対話的なコマンドを呼び出した際の前置引数を、数字とし
て解釈するための関数です。関数を定義する際の `(interactive "p")` に相当する関
数です。前置引数は大体の場合には整数しか入力できませんので、整数ならそのまま評
価して、整数以外なら（ `t` でも `nil` でも float でも）1になる仕様のようです。
float が符号にかかわらず1になるのはちょっと奇妙な感じですね。

ただし `-` や `(16)` は例外です。 `-` は `C--` (\`negative-argument') を押した
時の、 `(16)` は `C-u` (\`universal-argument') を2回押した時の前置引数に相当し
ます。それぞれ-1、16と評価されます。詳細は

```emacs-lisp
(Info-goto-node "(elisp)Prefix Command Arguments")
```

を評価して info を読んでください。

というわけで引数が `t` ならマイナーモードは有効になります。そしたら "With a
prefix argument ARG, enable Tool Bar mode if ARG is positive, and disable it
otherwise." じゃなくて "With a prefix argument ARG, disable Tool Bar mode if
ARG is **non-positive integer**, and enable it otherwise." の方が正確な気がしま
すけどね。


## マイナーモードのトグル {#マイナーモードのトグル}

マイナーモードは対話的に呼ぶとトグル動作になることは、皆さんご存知かと思います。
Emacs Lisp コードでマイナーモードをトグルにするためには、引数に \`toggle' を指
定するか、\`call-interactively' で対話的に呼ぶかになります。

```emacs-lisp
(tool-bar-mode 1)                   ; => t
(tool-bar-mode 'toggle)             ; => nil
(tool-bar-mode 'toggle)             ; => t
(call-interactively 'tool-bar-mode) ; => nil
(call-interactively 'tool-bar-mode) ; => t
(tool-bar-mode nil)                 ; => t
(tool-bar-mode nil)                 ; => t
```

実は以前は `(tool-bar-mode nil)` でもトグル動作になっていました。しかし最近に
なって `(tool-bar-mode nil)` は無条件でマイナーモードを有効にするように変更さ
れました。

Emacs News にこんな記述があります。

```text
* Incompatible Lisp Changes in Emacs 24.1

** Passing a nil argument to a minor mode function call now ENABLES
the minor mode unconditionally.  This is so that you can write e.g.

 (add-hook 'text-mode-hook 'foo-mode)

to enable foo-mode in Text mode buffers, removing the need for
`turn-on-foo-mode' style functions.  This affects all mode commands
defined by `define-minor-mode'.  If called interactively, the mode
command still toggles the minor mode.
```

フックに引っ掛けるときの利便性のための変更のようです。上の評価は Emacs 24.2 で
やっているので、 `(tool-bar-mode nil)` はトグルじゃなくて有効操作になっていま
す。

というわけで、くれぐれも `(tool-bar-mode nil)` でモードがトグルするとか無効に
なると思ってはいけません。自分は設定ファイルでモードの有効無効を設定する際に
`(tool-bar-mode t)` と `(tool-bar-mode 0)` と書くのは対称性がなくて好きじゃな
いので、 `(tool-bar-mode 1)` と `(tool-bar-mode 0)` と書くようにしてます。


## まとめ {#まとめ}

-   `(hoge-mode t)` でマイナーモードは有効になります。
-   モードを無効にしたい時は `(hoge-mode arg)` の arg を0以下の整数にしましょう。
    `(hoge-mode nil)` ではモードは無効になりません。
-   Emacs 24.1 以上なら `(hoge-mode nil)` はトグル動作ではなく、モードが有効にな
    ります。なので、もしなにかのモードのフックに引っ掛けてマイナーモードを有効に
    する

    ```emacs-lisp
    (add-hook 'huga-mode-hook (lambda () (hoge-mode 1)))
    ```

    みたいなコードがある場合は

    ```emacs-lisp
    (add-hook 'huga-mode-hook 'hoge-mode)
    ```

    とすっきり書きなおすことができます。
-   docstring の記述はやや不正確。


## <span class="timestamp-wrapper"><span class="timestamp">[2013-02-11 Mon 10:58] </span></span> 追記 {#追記}

対称性を考えると、モードの有効無効を `(hoge-mode 1)` と `(hoge-mode -1)` で書
く人もいらっしゃるようです。Emacs 24.2の標準添付のライブラリでは0派と-1派のど
ちらが多いか調べてみました。参考に1の数も書いておきます。

```text
$ find local/share/emacs/24.2/lisp -name "*.el.gz" | xargs zgrep -e '([^ ]\+-mode 1)' | wc -l
193
$ find local/share/emacs/24.2/lisp -name "*.el.gz" | xargs zgrep -e '([^ ]\+-mode 0)' | wc -l
57
$ find local/share/emacs/24.2/lisp -name "*.el.gz" | xargs zgrep -e '([^ ]\+-mode -1)' | wc -l
80
```

おお、-1派の方が多いようですね。自分も-1派に転じてみましょうか。

番外編で `t` と `nil` です。

```text
$ find local/share/emacs/24.2/lisp -name "*.el.gz" | xargs zgrep -e '([^ ]\+-mode t)' | wc -l
100
$ find local/share/emacs/24.2/lisp -name "*.el.gz" | xargs zgrep -e '([^ ]\+-mode nil)' | wc -l
99
```

ただこれは `(let ((hoge-mode nil))` みたいに変数に束縛しているケースもたくさん
含まれている（特に `nil` ）ので、あくまで参考です。