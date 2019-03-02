+++
title = "org-octopressで記事投稿テスト"
author = ["Tsunenobu Kai"]
date = 2013-12-15
tags = ["Emacs", "org-mode"]
draft = false
+++

## org-modeで記事を書く {#org-modeで記事を書く}

-   [Octopress の記事を org-mode で - Quickhack Diary](http://quickhack.net/nom/blog/2013-05-01-org-octopress.html)

を参考にしました。

<!--more-->


## テストテスト {#テストテスト}


### 整形済み {#整形済み}

単純な整形済みテキストは以下のように書く。（# は全角文字になってます）

```text
＃+BEGIN_EXAMPLE
$ echo hogehoge
hogehoge
＃+END_EXAMPLE
```

出力は

```text
$ echo hogehoge
hogehoge
```

-n オプションで行番号を付加できます。

```text
＃+BEGIN_EXAMPLE -n
一人目
二人目
三人目
＃+END_EXAMPLE
```

出力は

{{< highlight text "linenos=table, linenostart=1">}}
一人目
二人目
三人目
{{< /highlight >}}

css で行番号の部分はコピペできないようにしたけど、もっと見た目でコピーできない
ということを伝えたい。


### コードブロック {#コードブロック}

ハイライト付きコードブロック（C++ の場合）

```text
#+BEGIN_SRC java
package com.example;

public class Main {
  public static void main(String[] args) {
    System.out.println("Hogehoge Hugahuga");
  }
}
#+END_SRC
```

出力は

```java
package com.example;

public class Main {
  public static void main(String[] args) {
    System.out.println("Hogehoge Hugahuga");
  }
}
```

整形済みと同様に行番号を付加できます

```text
#+BEGIN_SRC java -n
package com.example;

public class Main {
  public static void main(String[] args) {
    System.out.println("Hogehoge Hugahuga");
  }
}
#+END_SRC
```

出力は

{{< highlight java "linenos=table, linenostart=1" >}}
package com.example;

public class Main {
  public static void main(String[] args) {
    System.out.println("Hogehoge Hugahuga");
  }
}
{{< /highlight >}}


### リスト {#リスト}

```text
- hoge
- huga
- untra
```

出力は

-   hoge
-   huga
-   untra


### 数字付きリスト {#数字付きリスト}

```text
1. hoge
2. huga
3. untra
```

出力は

1.  hoge
2.  huga
3.  untra


### マークアップ {#マークアップ}

いろいろな記法で文字を装飾することができます。

```text
*太字*, /斜体/, _下線_, +取り消し線+, =code=, ~verbatim~
```

出力は **太字**, _斜体_, <span class="underline">下線</span>, ~~取り消し線~~, `code`, `verbatim` 。あれ、太字
と斜体にならないな。

ただしマークアップ記法の前後は半角スペースや半角コンマなどの区切り文字でないと
マークアップされない。最後の2つの=code=や~verbatim~は等幅フォントで表示したい
時や、文字通り出力したい場合などに便利。


### 表 {#表}

| hoge   | ほげ |
|--------|----|
| huga   | ふが |
| untara | うんたら |


### 画像 {#画像}

画像を表示したい場合には、単純に画像へのファイルのリンクを貼ればよい。

```text
[[file:/images/google-map.jpg]]
```

出力は <br />
![](/images/google-map.jpg)
<br />
画像がローカルのファイルの場合、org2blog が自動的に画像をアップロードしてくれる。

`#+ATTR_HTML` で画像に属性を追加することができるので、インライン画像の表示幅
なども調節できる。

```text
#+CAPTION: 都庁付近の地図
#+ATTR_HTML: :alt 都庁付近の地図 :title 都庁付近の地図 :width 320
[[file:/images/google-map.jpg]]
```

出力は <br />

{{< figure src="/images/google-map.jpg" alt="都庁付近の地図" caption="Figure 1: 都庁付近の地図" title="都庁付近の地図" width="320" >}}

リンク文字列付きでリンクすれば、画像へのリンクが貼られたテキストになる。

```text
[[file:/images/google-map.jpg][ぐーぐるまっぷ]]
```

出力は [ぐーぐるまっぷ](/images/google-map.jpg)。

次は Web 上の画像をインライン表示してみる。

```text
#+ATTR_HTML: :alt Emacs の起動画面 :title ヌーヌー :width 400
[[https://www.gnu.org/software/emacs/tour/images/splash.png]]
```

出力は <br />

{{< figure src="https://www.gnu.org/software/emacs/tour/images/splash.png" alt="Emacs の起動画面" title="ヌーヌー" width="400" >}}

リンク文字列のほうに画像の URL を書けば、画像をインライン表示させつつ画像をク
リックして他の URL に飛ぶこともできる。

```text
#+CAPTION: A Guided Tour of Emacs
#+ATTR_HTML: :alt Emacs の起動画面 :title ヌーヌー :width 400
[[https://www.gnu.org/software/emacs/tour/][http://www.gnu.org/software/emacs/tour/images/splash.png]]
```

出力は <br />

{{< figure src="https://www.gnu.org/software/emacs/tour/images/splash.png" alt="Emacs の起動画面" caption="Figure 2: A Guided Tour of Emacs" title="ヌーヌー" width="400" link="https://www.gnu.org/software/emacs/tour/" >}}


### gist のソースコードを埋め込み {#gist-のソースコードを埋め込み}

Octopressはデフォルトでgistに対応している。（波括弧は全角になっているが、本来
半角）

```text
｛% gist gist_id [filename] %｝
```

と書くことでgistコードを埋め込むことができる。このような形になるように
org-mode のマクロを定義しておく。

```org
#+MACRO: gist ｛% gist $1 $2 %｝
```

このマクロを定義した上で以下のように書くと、

```text
｛｛｛gist(2988755,ThisIsTestOfGist.sh)｝｝｝
```

出力は

となる。あとはCSS次第か。


### twitter のツイートを埋め込み {#twitter-のツイートを埋め込み}

WordPress3.4 で twitter の埋込みに対応したらしい。独立した行に URL を書けばい
いらしい。

```text
https://twitter.com/kai2nenobu/status/217381492052082689
```

出力は <br />
<https://twitter.com/kai2nenobu/status/217381492052082689> <br />
どうも org-mode の方が URL をリンクに変換してしまうので、うまくいかない模様。

他の書き方もあるのでやってみる。この書き方だとオプションで幅や高さを決めること
もできる。平文で2行目を書いてしまうと、やはり URL がリンクになってしまうので
HTML ブロックで囲むのがよさそう。記号は一部大文字になっています。

```text
＃+BEGIN_HTML
［embed］https://twitter.com/kai2nenobu/status/217381492052082689［/embed］
＃+END_HTML
```

出力は

<div class="HTML">
  <div></div>

[embed]<https://twitter.com/kai2nenobu/status/217381492052082689>[/embed]

</div>