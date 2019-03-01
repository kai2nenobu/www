+++
title = "Emacs は外部 elisp がなくても強い (Emacs Advent Calendar jp: 2011 5日目)"
author = ["Tsunenobu Kai"]
date = 2011-12-05
tags = ["Emacs"]
draft = false
+++

この記事は [Emacs Advent Calendar jp: 2011](http://atnd.org/events/21982) の5日目です。 4日目は HKey さんの
[パスをまとめよう](http://hke7.wordpress.com/2011/12/03/%E3%83%91%E3%82%B9%E3%82%92%E3%81%BE%E3%81%A8%E3%82%81%E3%82%88%E3%81%86-emacs-advent-calendar-jp-2011/) でした。 6日目は id:kiwanami さんです。kiwanami さんの elisp
にはいつもお世話になっているので、とても楽しみです。

今回は、Emacs に標準で入っているけどあまり知られていなさそうな便利機能、あるい
は数行で書けるカスタマイズや便利コマンドを紹介したいと思います。

紹介するのは以下の機能です。

-   連続 pop-mark
-   プレフィックスキーを増やす
-   パスを1階層ずつ削除
-   相対的なカーソル位置を動かさないスクロール

について順番に説明していきたいと思います。

<!--more-->


## 連続 pop-mark {#連続-pop-mark}

Emacs のバッファでは、C-SPC あるいは C-@ (\`set-mark-command') でカーソルの位置に
マークを付けることができます。このマークとカーソルの間が選択領域になり、その
領域をコピーしたりキルしたりする機能はみんな使っていることと思います。

\`set-mark-command' 以外にも \`isearch' や \`beginning-of-buffer' を実行した際な
どに人知れずマークを変更しているコマンドがあります。これらのコマンドでマークが
変更されるたびに、古いマークは \`mark-ring' にスタックのように保存されていきま
す[^fn:1]。

この \`mark-ring'、実は C-u C-SPC (\`pop-mark' というコマンドを実行) によって古い
マークを順番に辿っていくことができます。具体的に使い方を見てみましょう。

```text
|This is a test sentence. `pop-mark' can restore position of cursor.
```

Emacs のバッファ内に上記のような文章が、ありカーソルが | の位置にあるとします。
ここで C-s ('isearch-forward\`) で "sentence" を検索し RET で検索を終了すると
カーソル位置は

```text
This is a test sentence|. `pop-mark' can restore position of cursor.
```

このように移動します。この時、\`isearch' によって文頭の位置にマークが変更され
ています。さらに "cursor" で検索して RET すると

```text
This is a test sentence. `pop-mark' can restore position of cursor|.
```

こうなります。先程と同様に \`isearch' によって "sentence" と "." の間にマーク
が変更されています。ここで C-u C-SPC をタイプすると

```text
This is a test sentence|. `pop-mark' can restore position of cursor.
```

このようにカーソル位置がマークの位置に戻ります。これが \`pop-mark' の機能です。
さらにもう一回 C-u C-SPC をタイプすると

```text
|This is a test sentence. `pop-mark' can restore position of cursor.
```

\`kill-ring' から1つ前のマークを取り出してきて、その位置にカーソルを移動します。
すなわち最初のカーソル位置に戻ることになります。このようにして、古いマークをど
んどん辿っていくことができます。ソースを見ている際にある関数が使われていて、そ
の関数を \`isearch' してその定義の場所まで移動した後また元の場所に戻ってくる、
といったようなことがことがこの機能を使えば簡単に出来ます。

しかし連続でマークを辿る際に C-u C-SPC C-u C-SPC ... を連続で入力するのはめん
どくさい。ので

```emacs-lisp
;; enable to pop `mark-ring' repeatedly like C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)
```

この設定を init.el に書いておくと C-u C-SPC C-SPC C-SPC... のように C-SPC を連
続で入力するだけで、連続でマークを辿れるようになります。\`pop-mark' は非常に簡
便かつ便利な機能なので、ぜひ使ってみてください。


## プレフィックスキーを増やす {#プレフィックスキーを増やす}

Emacs をカスタマイズし始め、自分で独自のキーバインドを増やし始めると割り当てる
キーが不足してくるものです。これを解決するには、複数の機能をまとめたコマンドを
使う、キーを増やすような外部 elisp（[key-chord.el](http://www.emacswiki.org/emacs/KeyChord) とか）といった対策があると思
います。が、ここではもっと簡単な、いらない1ストロークのキーをプレフィックスキー
にして、2ストロークキーを増やす方法を説明したいと思います。

まず、おそらく一番使用頻度が低いであろう C-q (\`quoted-insert') を潰して、プレ
フィックスキーにすることにします。最も単純には、以下のようにすれば2ストローク
キーを定義することができます。

```emacs-lisp
(define-key global-map (kbd "C-q") nil)
(define-key global-map (kbd "C-q" "C-q") 'quoted-insert)
(define-key global-map (kbd "C-q" "C-t") 'toggle-truncate-lines)
```

しかしこれは直接2ストロークを指定しているので、プレフィックスキーを C-q から
C-z したいといったことがめんどくさかったりします。そこで自分独自のキーマップを
定義して、そのキーマップをプレフィックスキーに割り当てる方法のほうがなにかと便
利です。その場合はこのようになります。

```emacs-lisp
(defvar my-original-map (make-sparse-keymap) "My original keymap binded to C-q.")
(define-key global-map (kbd "C-q") my-original-map)
```

新しいキーマップは \`make-sparse-keymap' で作成することができるので、それを好き
な名前で定義します。そのキーマップを C-q に割り当てれば C-q がプレフィックスキー
になります。\`define-key' の最後の引数はキーマップそのものを指定するのでクオー
トしないことに注意してください。あとは \`my-original-map' にコマンドを割り当て
ればどんどん2ストロークキーが増えていきます。

上では、プレフィックスキーに直接キーマップを割り当てましたが、キーマップを呼
び出すための関数を割り当ててもよいです。こんな感じです。

```emacs-lisp
(defvar my-original-map (make-sparse-keymap) "My original keymap binded to C-q.")
(defalias 'my-original-prefix my-original-map)
(define-key global-map (kbd "C-q") 'my-original-prefix)
```

2行目が加わっただけです。\`defalias' で \`my-original-prefix' の定義をキーマップ
にします。これで、普通のコマンドのように \`define-key' でプレフィックスキーに割
り当てられます[^fn:2]。

2番目と3番目の方法の違いは、\`describe-bindings' で表示される名前が変わってき
ます。2番目の場合、C-q は Prefix Command と表記されます。関数が割り当てられて
いないので名前がわからないということでしょう。3番目の方であれば、C-q は
my-original-prefix と表記されることになり、なんのためのプレフィックスかが一目
瞭然となります。どちらを選ぶかは好みになるでしょうか。

また、\`define-prefix-command' を使えば \`defvar' と \`defalias' をひとまとめにす
ることもできます。

```emacs-lisp
(define-prefix-command 'my-original-map)
(define-key global-map (kbd "C-q") 'my-original-map)
```

キーマップを保持する変数名と、それを呼び出す関数名が同じ \`my-original-map' に
なりますが、それが気にならなければこの方法でもいいと思います。しかし、これだと
DOCSTRING が書けないので個人的には3番目の方法で書いています。

自分のオリジナルキーマップの一部を晒してみます。

```emacs-lisp
;; original key map (bind to C-q)
(defvar my-original-map (make-sparse-keymap)
  "My original keymap binded to C-q.")
(defalias 'my-original-prefix my-original-map)
(define-key global-map (kbd "C-q") 'my-original-prefix)
(define-key my-original-map (kbd "C-q") 'quoted-insert)
(define-key my-original-map (kbd "C-t") 'toggle-truncate-lines)
(define-key my-original-map (kbd "C-l") 'linum-mode)
(define-key my-original-map (kbd "C-r")
  '(lambda () (interactive) (revert-buffer nil t t)))
(define-key my-original-map (kbd "C-c") 'column-highlight-mode)
(define-key my-original-map (kbd "TAB") 'auto-complete) ; あえて手動で補完したい時
```

おおむねトグル系のコマンドや、使用頻度は高くないけどたまーに必要なものを割り当
てています。C-q C-q の \`quoted-insert' は特殊文字を入力する際に必要になります。
C-q C-t の \`toggle-truncate-lines' はバッファの折り返しをトグル、C-q C-l は行
番号の表示をトグルします。この2つは結構頻繁に切り替えたいので、割り当てておく
と便利です。

C-q C-r は警告なしで \`revert-buffer' します。Dropbox で共有したファイルを編集
していると、別の場所で編集したファイルを開きなおすことがあるので割り当てまし
た。最後の2つは外部 elisp の関数です。\`column-highlight-mode' はカーソルの
あるカラムをハイライトします。elisp を書く際にインデントが揃っているか確認す
るのに便利です。\`auto-complete' は自動的に補完をしてくれる関数ですが、たまに
手動で補完を開始したい時があるので割り当てています。

おまけですが、すでに定義されているキーマップを別のプレフィックスキーに割り当
てることも当然出来ます。

```emacs-lisp
(define-key global-map (kbd "C-4") 'ctl-x-4-prefix)
(define-key global-map (kbd "C-5") 'ctl-x-5-prefix)
(defalias 'ctl-x-r-prefix ctl-x-r-map)
(define-key global-map (kbd "S-C-r") 'ctl-x-r-prefix)
```

こうすると、C-x 4 f (\`find-file-other-window') や C-x r t
(\`string-rectangle') といった長ったらしい3ストロークのキーを2ストロークで入力
できるようになります。特に C-x 4 の other-window 系の関数はが2ストロークで使
えるのは超絶便利です[^fn:3]。


## パスを1階層ずつ削除 {#パスを1階層ずつ削除}

\`find-file' などでプロンプトにパスを入力する際、現在のディレクトリがプロンプト
にあらかじめ入力されておりカーソルがその右端に置かれている場合が多くあります。

同じディレクトリのファイルを入力する場合はいいのですが、他のディレクトリのファ
イル名を入力したい場合もあり、いちいちパスの階層を BACKSPACE などで削除するの
も手間です。

というわけで、パスを1階層ずつ削除するコマンドを書きました。

```emacs-lisp
(defun my-minibuffer-delete-parent-directory ()
  "Delete one level of directory path."
  (interactive)
  (let ((current-pt (point)))
    (when (re-search-backward "/[^/]+/?" nil t)
      (forward-char 1)
      (delete-region (point) current-pt))))
(define-key minibuffer-local-map (kbd "M-^") 'my-minibuffer-delete-parent-directory)
```

このコマンドでカーソルの左にある "_" までを削除してくれます。例えば、プロンプ
トで "~_.emacs.d/site-lisp/migemo.el" が入力されている状態で3回コマンドを実行
すると以下のようになります。

```text
Find File: ~/.emacs.d/site-lisp/migemo.el|
Find File: ~/.emacs.d/site-lisp/|
Find File: ~/.emacs.d/|
Find File: ~/|
```

短いコマンドですが、効果は上々です。パスが "~/" だけになったときに上の階層にさ
かのぼれないなどの問題はありますが、自分では非常に対症療法な対策しか思いつかな
いので、ハックしてくれる方募集中です。

\`minibuffer-local-map' に割り当てればプロンプト中で使うことができます。M-^ に
割り当てたのは、\`global-map' で M-^ に割り当てられている \`delete-indentation'
とイメージが似てるなーと思ったらからです[^fn:4]。
機能的には <C-backspace> でもイメージしやすいかもしれません。


## 相対的なカーソル位置を動かさないスクロール {#相対的なカーソル位置を動かさないスクロール}

C-v (\`scroll-up') をタイプするとバッファ内の画面を上にスクロールさせることがで
きます。この時カーソルはウィンドウの一番上に移動してしまいます。これでは C-v
でバッファ内の目的の場所まで画面をスクロールした後、ウィンドウの一番上から
C-n などで目的の行まで行移動をすることになります。

この挙動は個人的にあまり好みではありませんでした。編集しているときは大概カーソ
ルはウィンドウの真ん中辺りにあるのだから、カーソルはその位置のままスクロールし、
真ん中から細かい行移動をする方が効率的かなーと思いました。Vi/Vim の C-d, C-u が
ちょうどカーソルを動かさずに画面をスクロールします。

Emacs にはそんなコマンドはないようなので、Emacs Lisp の練習がてら自分で書いて
みました。

まず相対的なカーソル位置を保存しないといけないので、

-   ウィンドウ内でカーソルが何行目にあるかを取得する関数

が必要になります。またバッファが折り返されている場合、論理行数ではなく物理行数
を数える必要があります。そのためには

-   文字列の幅（カラム数）を返す関数

が必要になります。その2つの関数が以下のようになります。

```lisp
(defun my-count-lines-window ()
  "Count lines relative to the selected window. The number of line begins 0."
  (interactive)
  (let* ((window-string (buffer-substring-no-properties (window-start) (point)))
	 (line-string-list (split-string window-string "\n"))
	 (line-count 0)
	 line-count-list)
    (setq line-count (1- (length line-string-list)))
    (unless truncate-lines      ; consider folding back
      ;; `line-count-list' is list of the number of physical line which each logical line has.
      (setq line-count-list (mapcar '(lambda (str)
				       (/ (my-count-string-columns str) (window-width)))
				    line-string-list))
      (setq line-count (+ line-count (apply '+ line-count-list))))
    line-count))

(defun my-count-string-columns (str)
  "Count columns of string. The number of column begins 0."
  (with-temp-buffer
    (insert str)
    (current-column)))
```

\`my-count-lines-window' でカーソル位置がウィンドウ内の何行目かがわかります。折
り返しの境界近くにカーソルがあると1ぐらいずれるかもしれませんが、大体の場合は
大丈夫のはずです。

この2つの関数さえできてしまえば、あとは \`scroll-down' がカーソル位置を保つように
アドバイスします。

```lisp
(defadvice scroll-up (around scroll-up-relative activate)
  "Scroll up relatively without move of cursor."
  (let ((line (my-count-lines-window)))
    ad-do-it
    (move-to-window-line line)))

(defadvice scroll-down (around scroll-down-relative activate)
  "Scroll down relatively without move of cursor."
  (let ((line (my-count-lines-window)))
    ad-do-it
    (move-to-window-line line)))
```

これで、C-v でカーソル移動がしなくなり心持ち負担が減ったように思います。

ついでに、先ほど話しに出した Vi/Vim の C-d, C-u にあたる半画面スクロールや
1行ずつスクロールするキーバインドもあるとたまに便利だったりします。

```lisp
(define-key global-map (kbd "H-u")
  '(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(define-key global-map (kbd "H-d")
  '(lambda () (interactive) (scroll-up (/ (window-height) 2))))

(define-key global-map (kbd "H-n") '(lambda (arg) (interactive "p") (scroll-up arg)))
(define-key global-map (kbd "H-p") '(lambda (arg) (interactive "p") (scroll-down arg)))
```

このように細かい挙動を自分の好きにカスタマイズできるのが、やはり Emacs の強い
ところだと思います。

[^fn:1]: ちなみに、\`isearch' は C-g で検索を終了すると、カーソルが検索を開始する 前の位置に戻りマークは変更されません。C-g 以外の要因で検索が終了した場合のみマー クが保存されます
[^fn:2]: \`ctl-x-4-prefix' や \`ctl-x-5-prefix' は subr.el でこのようにし て定義されています
[^fn:3]: ただし端末上では C-4 や S-C-r といったキーが使えないの が残念です
[^fn:4]: この話とは全く関係有りませんが、 \`delete-indentation' も便利なコマンドなので使ってみることをお勧めします