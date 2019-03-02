+++
title = "Emacs で自動的にキーバインドをポップアップする guide-key"
author = ["Tsunenobu Kai"]
date = 2012-12-03
tags = ["Emacs", "guide-key"]
draft = false
+++

[Emacs Advent Calendar 2012 - Qiita](http://qiita.com/advent-calendar/2012/emacs) の3日目です！ [Qiita の投稿](http://qiita.com/items/16bd5cb65be18e804c63)では軽く設定を説
明しただけなので、こちらではもう少し詳しく説明します。

自動的にキーバインドをポップアップするライブラリ、[guide-key.el](https://github.com/kbkbkbkb1/guide-key) を自作したので
紹介したいと思います。

<!--more-->


## 開発の動機 {#開発の動機}

Emacs を利用する場合の大きな障壁の1つにキーバインドの覚えにくさがあるでしょう。
1ストロークのキーバインドは頻繁に使いますし、まだ覚えやすいですが、2ストローク
以上のキーは頻繁に使うものしか覚えていない、という人も多いのではないでしょうか。
ましてや新しく導入した外部ライブラリのキーバインドなどは覚えるのが億劫になりま
す。Emacs には標準で `describe-key` や `describe-bindings` などキーバインドを
調べる機能もありますが、必ずしも使いやすくありませんし、能動的にコマンドを実行
しないといけません。

この問題を解決する外部ライブラリとして有名なのが [one-key.el](http://emacswiki.org/emacs/one-key.el) です。このライブラ
リを使えば、プレフィクスキーを押した際に、そのプレフィクスに続くキーを自動
的に（別ウィンドウに）表示してくれます。下図は one-key.el を導入して `C-x r`
を押したときの様子です。

{{< figure src="/images/one-key-example.png" >}}

しかし one-key.el にもいくつか問題があります。

-   プレフィクスに続くキー（テンプレート）を自分で記述しないといけないません。
    主要なプレフィクスキーのテンプレートはあらかじめ用意してありますが、自分で
    書く場合は手間がかかります。その手間を軽減してくれるジェネレータを
    rubikitch さんが[こちら](http://d.hatena.ne.jp/rubikitch/20090127/onekey)で書いてくれていますが、やはりめんどくさい。
-   キーマップのキーバインドを変更した場合、テンプレートの方も手動で変更しなけれ
    ばなりません。テンプレートが自動的にキーマップに追随してくれたらいいのに。
-   one-key.el はプレフィクスキーに割り当ててあるコマンドを置き換えます。つま
    りデフォルトでは `C-x r` に割り当てられている `ctl-x-r-prefix` を上書きして
    しまうのでいろいろ問題が出てきます。例えば `describe-key` で `C-x r t` に割
    り当てられているコマンドを調べようとすると、 `C-x r` に割り当てられている
    one-key のコマンドが先に出てきてしまい、直接 `C-x r t` を調べることができな
    くなってしまいます。

以上の問題点を解決した [guide-key.el](https://github.com/kbkbkbkb1/guide-key) というのを作ってみました。キーマップに自
動的に追随してくれますし、コマンドを上書きするようなこともありません。


## guide-key.el の導入 {#guide-key-dot-el-の導入}

[MELPA](http://melpa.milkbox.net/) に登録しているので、package.el を使ってインストールできます。Emacs24 の
人や package.el を自分で入れている人は init.el に

```emacs-lisp
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

と設定すると、MELPA に登録されているパッケージを見ることができます。 `M-x
list-packages` を実行してパッケージをリストアップし、 `i` で guide-key を選
択、~x~ で実行することにより guide-key がインストールされます。guide-key は
[popwin](https://github.com/m2ym/popwin-el) に依存しているので、package.el でインストールすれば自動的に popwin もイ
ンストールされます。

{{< figure src="/images/guide-key-package-install.png" alt="packageでguide-key.elをインストールする図" title="guide-key.elのインストール" width="640" >}}

package.el がない人は [m2ym/popwin-el](https://github.com/m2ym/popwin-el) と [kai2nenobu/guide-key](https://github.com/kbkbkbkb1/guide-key) から popwin.elと
guide-key.el を直接ダウンロードしてきて、適当に `load-path` が通っているところ
に保存してください。


## 使い方 {#使い方}

`guide-key/guide-key-sequence` にポップアップして欲しいキーシーケンス（プレ
フィックスキー）を登録します。init.el に以下のように設定してください。

```emacs-lisp
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; guide-key-mode を有効にする
```

これで `C-x r` や `C-x 4` を押した時に、次に続くキーを表示するウィンドウが自動
的にポップアップされます。実際に `C-x r` に押した様子はこのようになります。

{{< figure src="/images/guide-key-example.png" >}}

guide-key は特定の正規表現に当てはまるコマンド名に色をつけることができます。
`C-x r` には rectangle 系と register 系のコマンドがあります。いま rectangle 系
のコマンドに色をつけて目立たせたいとすると以下のように設定します。

```emacs-lisp
(setq guide-key/highlight-command-regexp "rectangle")
```

また `C-x r` に続くキーバインドを変更した時に、自動的に追随するかを確かめるた
めにキーバインドを追加します。

```emacs-lisp
(global-set-key (kbd "C-x r 3 a") 'hoge)
(global-set-key (kbd "C-x r 4") 'ctl-x-4-prefix)
```

この設定で `C-x r` を押すとこうなります。

{{< figure src="/images/guide-key-example2.png" >}}

このように rectangle 系のコマンドだけが色付けされます。これでコマンドを探しや
すくなりますし、キーバインドを覚えやすくなります。またキーバインドの変更に追随
して、 `ctl-x-4-prefix` や `Prefix Command` が表示されています。

rectangle 系、register 系どちらも色付けしたければ

```emacs-lisp
(setq guide-key/highlight-command-regexp "rectangle\\|register")
```

のように、適当に正規表現を設定してください。またプレフィクスキー（"prefix"
という正規表現に一致するコマンド）にも自動的に色付けがされます。


## 特定のモードで設定を追加 {#特定のモードで設定を追加}

こういったキー入力を補助して欲しい場面は、おそらく新しく導入したモードに独自の
キーバインドがある場合でしょう。「さっきマニュアルを見たのに、もうキーバインド
を忘れた」ということが起きないように、以下では guide-key を使って特定のモード
に対して設定を追加します。

`guide-key/add-local-guide-key-sequence`
と~guide-key/add-local-highlight-command-regexp~ を使うと現在のバッファの変数
のみを変更できます。これを特定のモードのフックと組み合わせれば、特定のモードに
対して設定を追加できます。

例えば org-mode を例にとって見ると、以下のような設定になります。

```emacs-lisp
(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
```

この設定をした後、org-mode のバッファで `C-c C-x p` を押して
`org-set-property` が実行される様子が以下の図です。

{{< figure src="/images/guide-key-example-org-anime.gif" >}}

色付けする正規表現に "org-" を追加しているので、ほとんどのコマンドが色付けされ
ています。これだとあまり意味が無いので、覚えたいコマンド群のみが色付けされるよ
うに、もっと絞り込める正規表現に好みで変更してください。

メジャーモードでもマイナーモードでもフックさえあれば同様のことができるので、
お好きなモードで試してみてください。


## その他詳細 {#その他詳細}

guide-key の動作は簡単で、定期的に現在入力されているキーシーケンスをポーリング
で調べ、 `guide-key/guide-key-sequence` に含まれる場合は次に続くキーをポップアッ
プするだけです。次に続くキーは `describe-buffer-bindings` でその度に取得してい
るので、動的にキーバインドの変更に追随できます。キーを入力するたびに実行される
ようなフックがあればポーリングでなくても良かったんですが、そんなフックは存在し
ないようです。最小単位のフックは、コマンドを実行するときの
`post-command-hook` か `pre-command-hook` のなのかな。

現在入力中のキーシーケンスは `this-command-keys-vector` で取得することができま
す。Emacs 内部でのキーイベントの表現は、[Emacsでキーボードイベントを扱う方法ま
とめ - むしゃくしゃしてやった](http://d.hatena.ne.jp/uk-ar/20120213/1329138385)を参考にしました。プレフィクスキー `C-x r` の文
字列表現は "C-x r" か "\C-xr" の2種類ありますが、
`guide-key/guide-key-sequence` はどちらの表現でも受け付けますし、混在でも構い
ません。

ポップアップウィンドウの制御はすべて popwin に任せています。自分でウィンドウ構
成の保持や、ポップアップする位置を調整したりする部分を自分で書く必要がなく、とっ
ても楽にコードが書けました。popwin の作者の [m2ym](http://cx4a.blogspot.jp/) さんには感謝を申し上げます。

guide-key の動作を制御する変数などを説明しておきます。

-   `(guide-key-mode ARG)`: `guide-key-mode` はマイナーモードで実装されています。
    対話的に実行すればトグル動作になるので、一時的に有効無効を切り替える場合は
    `M-x guide-key-mode` を実行してください。グローバルマイナーモードなので、特
    定のバッファのみで有効にする、といったような動作はできません。全バッファ共通
    で有効か無効かのどちらかです。
-   `guide-key/popup-window-position`: ポップアップするウィンドウの位置を制御す
    る変数です。 `right`, `bottom`, `left`, `top` のいずれかを指定してください。
    デフォルトは `right` です。
-   `guide-key/polling-time`: 入力されているキーシーケンスをポーリングする間隔を
    制御する変数です。デフォルトは0.1です（秒単位）。キーを押してすぐさまポップ
    アップされるのが嫌な場合は長くするといいでしょう。おそらく0.1でもほぼ一瞬で
    表示されるように感じると思います。0.01ぐらい短くしてもきちんと動作し、他の操
    作にも影響が無いことを確認していますが、0.1で大多数の人は問題ないと思います。

guide-key が動作することを確認している環境は以下のようになります。

-   Emacs 24.2, Ubuntu 12.04 or Windows 7 64bit
-   Emacs 23.3, Ubuntu 12.04 or Windows 7 64bit
-   Emacs 22.3, Windows 7 64bit

とにかく popwin が動けば guide-key も動作するはずです。ターミナル環境の Emacs
でも問題なく動作します。


## まとめ {#まとめ}

最後に guide-key の特徴をまとめておきます。

-   現在入力しているキーシケンスに続くキーを自動的にポップアップします。ポップアッ
    プさせたいプレフィクスを設定するだけで使えます。また動的にキーバインドを調
    べているので、キーバインドが変更されても動的に追随できます。
-   特定のコマンドを色付けすることできます。いま注目している機能に関するコマンド
    だけを色付けすることにより、キーバインドを探しやすくなり体で覚えることがで
    きます。
-   既存のコマンドを上書きしないので、 `describe-key` や `describe-bindings` な
    どに影響が出ません。

既知の問題点、欠点には以下のようなものがあります。

-   guide-key は次に続くキーバインドをすべて表示しようとするので、ポップアップウィ
    ンドウのサイズが大きくなりがちです。もし現在のフレームの大きさよりポップアッ
    プウィンドウの方が大きくなると、正常にポップアップされなくなります。なのでフ
    レームを大きくするか、キーバインドが少ないプレフィクスのみを設定してくださ
    い。キーバインドの多い `C-x` などをポップアップさせるのは、全くの初心者の人
    以外はあまりお勧めしません。将来的には、ポップアップするコマンドの方を個数や
    正規表現で制限する機能を追加するかもしれません。
-   またポップアップされるキーバインドが多すぎると、目視で目的のコマンドを探すこ
    とが難しくなります。ポップアップされるコマンドの個数、あるいは色付けされたコ
    マンドの個数が数個〜十数個ぐらいになるのが理想的かと思います。
-   キーバインドをポップアップさせようとした時、一瞬ポップアップされて一瞬で閉じ
    てしまうことがあります。popwin で制御されているウィンドウ（デフォルトだと
    Help バッファや Apropos バッファなど）を閉じた直後に起こることが多いですが、
    他の場面でもたまにあります。そういうときは C-g を連打したり、他のコマンドを
    実行して仕切りなおしてからもう一回プレフィクスキーを入力してみてください。
-   one-key ではコマンド名の代わりに、短い説明文字列を表示させることが出来ました。
    これは手動でテンプレートを作っているからこその利点です。guide-key では動的に
    キーバインドからコマンドを抽出してくるので、コマンド名しか表示させることが
    できません。

自分としてはキーバインドが変更されても追随するという事を最重要視して作りまし
た。guide-key を使えばもっとキーバインドを覚えやすくなると思いますので、みな
さんぜひ使ってみてください。