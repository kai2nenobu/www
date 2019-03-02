+++
title = "guide-keyの新機能棚卸"
author = ["Tsunenobu Kai"]
date = 2013-12-22
tags = ["Emacs", "guide-key"]
draft = false
+++

この記事は[.emacs Advent Calendar 2013](http://qiita.com/advent-calendar/2013/dot-emacs)の22日目の記事です。
[去年のアドベントカレンダー]({{< relref "2012-12-03-emacs-advent-calendar-2012-03" >}})で[guide-key](https://github.com/kbkbkbkb1/guide-key/blob/master/README.ja.org)という、キーバインドを自動表示する自作ラ
イブラリを紹介しました。この1年でいくつか機能を追加したので改めて紹介します。

<!--more-->


## 基本的な使い方 {#基本的な使い方}

基本的な使い方は[去年のアドベントカレンダー]({{< relref "2012-12-03-emacs-advent-calendar-2012-03" >}})の頃と変わってません。guide-keyは
MELPAに登録してあるので、 `M-x package-install guide-key` でインストールしてく
ださい。

`guide-key/guide-key-sequence` にポップアップして欲しいプレフィクスキーを登録
します。init.elに以下のように設定してください。

```emacs-lisp
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; guide-key-mode を有効にする
```

これで `C-x r` や `C-x 4` のプレフィクスキーを押した時に、次に続くキーバインド
を表示するウィンドウが自動的にポップアップされます。実際に `C-x r` に押した様
子は以下のようになります。

{{< figure src="/images/guide-key-example.png" >}}


## 追加機能 {#追加機能}


### ポップアップウィンドウの表示遅延 {#ポップアップウィンドウの表示遅延}

`guide-key/idle-delay` でポップアップウィンドウが表示されるまでの時間を調整で
きるようになりました。以前は入力されているキーをポーリングでチェックしていたの
で、プレフィクスキーを押してからポップアップウィンドウが表示されるまでの時間が
一定ではありませんでした。

`guide-key/idle-delay` はデフォルトで1秒になっているので、自分の好みに合わせて
調整してください。すでに覚えているキーバインドを素早く入力すればウィンドウはポッ
プアップされませんし、キーバインドを覚えておらず入力が途中で止まった時は
`guide-key/idle-delay` 秒後にウィンドウがポップアップされます。


### 特定のモードに関する設定 {#特定のモードに関する設定}

`guide-key/guide-key-sequence` に、特定のモードでのみポップアップしたいプレフィ
クスキーを設定できるようになりました。例えば以下のように設定してください。

```emacs-lisp
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4"                  ; すべてのバッファで有効
	(org-mode "C-c C-x")             ; org-modeのバッファのみ
	(outline-minor-mode "C-c @")     ; outline-minor-modeのバッファのみ
	))
```

メジャーモードが `org-mode` の場合 `C-c C-x` に続くキーバインドがポップアップ
されます。 `outline-minor-mode` が有効な場合、 `C-c @` に続くキーバインドがポッ
プアップされます。


### プレフィクスキーの再帰的なチェック {#プレフィクスキーの再帰的なチェック}

`guide-key/recursive-key-sequence-flag` がnon-nilの時、guide-keyは入力されたキー
を再帰的にチェックします。つまり `C-x 8 ^` が入力されている時、guide-keyは
`guide-key/guide-key-sequence` に `C-x 8` や `C-x` が含まれているかをチェック
します。

例えば以下のように設定した場合

```emacs-lisp
(setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/recursive-key-sequence-flag t)
```

`C-x r` や `C-x 8` など `C-x` に続くプレフィクスキーを押した際に、キーバイン
ドがポップアップされます。


### `key-chord` との連携 {#key-chord-との連携}

[key-chord](http://www.emacswiki.org/emacs/KeyChord)と連携することができるようになりました。key-chordは、2つのキーの同時
押しに対してコマンドを割り当てることができるようになるライブラリです。2つのキー
の同時押しをプレフィクスキーにすることもできます。

例えばhelm-modeには、helmのコマンドをまとめた `helm-command-map` というキーマッ
プが用意されています。以下のように設定すると、

```emacs-lisp
(require 'key-chord)
(key-chord-define-global (kbd ":h") helm-command-map)
```

`:` と `p` の同時押しがプレフィクスキーとなり、その後にもう1つキーを入力する
と対応するhelmコマンドが実行できます。

しかし `helm-command-map` にどのコマンドが割り当てられているかは、往々にして
忘れてしまうのでguide-keyでキーバインドをポップアップさせます。

同時押しのプレフィクスキーをポップアップしたい場合、
`guide-key/key-chord-hack-on` を実行する必要があります。その上で以下のように
`guide-key/guide-key-sequence` にkey-chordのプレフィクスキーを追加してください。

```emacs-lisp
(guide-key/key-chord-hack-on)
(setq guide-key/guide-key-sequence '("<key-chord> : h" "<key-chord> h :"))
```

`:` と `h` のどちらが先に押されるかわからないので、2通りの順番の両方を記述する
必要があります。実際に `:` と `h` を同時押しした様子が以下のようになります。

{{< figure src="/images/guide-key-key-chord.png" >}}

ちなみに `b` で実行できる `helm-resume` がとても便利です。

`guide-key/recursive-key-sequence-flag` がnon-nilの場合は、シンプルに設定でき
ます。

```emacs-lisp
(guide-key/key-chord-hack-on)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/guide-key-sequence '("<key-chord>"))
```

この設定の場合は、すべてのkey-chordのプレフィクスキーに続くキーバインドがポッ
プアップされます。

`guide-key/key-chord-hack-on` は `this-command-keys` と
`this-command-keys-vector` という原始的な関数をアドバイスしているため、 **危
険** な可能性があります。一応自分の環境では1年ほど動かして問題は発生していませ
んが、もし異常があるようでしたらレポートいただけると嬉しいです。


### ポップアップウィンドウのテキストサイズ調整 {#ポップアップウィンドウのテキストサイズ調整}

`guide-key/text-scale-amount` でポップアップするウィンドウのテキストサイズを調
整できるようになりました。テキストを大きくしたい場合は正の数、小さくしたい場合
は負の数に設定してください。

```emacs-lisp
(setq guide-key/text-scale-amount -1.5)
```

このように-1.5に設定して、実際にポップアップさせると以下のようになります。

{{< figure src="/images/guide-key-text-scale.png" >}}

小さいポップアップウィンドウで、たくさんのキーバインドが表示できているのが確認
できると思います。テキストサイズを大きくしすぎると、ポップアップウィンドウが大
きくなりすぎて正常に表示できないかもしれませんので注意してください。


## まとめ {#まとめ}

guide-keyの新機能を紹介しました。もし意見などありましたら
twitter([@kbkbkbkb1](https://twitter.com/kbkbkbkb1))やgithub([guide-key](https://github.com/kbkbkbkb1/guide-key))な
どにお願いします。