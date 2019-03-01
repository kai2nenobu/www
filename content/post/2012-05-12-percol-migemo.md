+++
title = "anything 的絞りこみコマンド percol が migemo 対応しました"
author = ["Tsunenobu Kai"]
date = 2012-05-12
tags = ["percol", "zsh", "tool"]
draft = false
+++

[先日のエントリ]({{< relref "2012-04-29-percol-introduction" >}})で [percol](https://github.com/mooz/percol) というとっても便利なコマンドを紹介しました。migemo 対
応してくれたら更に便利だなーと思っていたのですが、[mooz](http://d.hatena.ne.jp/mooz/) さんがばっちり
`--match-method` に migemo を追加して下さいました。
<!--more-->
percol で migemo を使うには [C/Migemo](http://www.kaoriya.net/software/cmigemo) と [PyMigemo](http://www.atzm.org/etc/pymigemo.html) を導入する必要があります。備忘
として Ubuntu 11.10 とWidnows7 での導入方法を記しておきます。めんどくさいのでイ
ンストール場所は全部デフォルトの場所で、migemo の辞書の文字コードは utf-8 を使
うことにします。

各ソフトウェアのバージョン

-   [C/Migemo 1.3](http://www.kaoriya.net/software/cmigemo)
-   [PyMigemo 0.3](http://www.atzm.org/etc/pymigemo.html)
-   [percol 0.0.2](https://github.com/mooz/percol)


## Ubuntu 11.04 に導入する {#ubuntu-11-dot-04-に導入する}

Ubuntu の環境は以下のようになってます。

-   Ubuntu 11.10
-   python 2.7.2
-   zsh 4.3.11

まずは cmigemo の導入です。[KaoriYaさん](http://www.kaoriya.net/software/cmigemo)からアーカイブをダウンロードしてきます。

```text
$ wget http://cmigemo.googlecode.com/files/cmigemo-default-src-20110227.zip
$ unzip cmigemo-default-src-20110227.zip
$ cd cmigemo-default-src
```

このソースの中の src/wordbuf.c は limits.h をインクルードし忘れているので、以
下のように修正します。

```text
$ diff -u src/wordbuf.c.bck src/wordbuf.c
--- src/wordbuf.c.bck   2012-05-11 21:52:44.006214700 +0900
+++ src/wordbuf.c       2012-05-11 19:40:39.310948000 +0900
@@ -9,6 +9,7 @@
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
+#include <limits.h>
 #include "wordbuf.h"

 #define WORDLEN_DEF 64
```

あとはビルドするだけです。

```text
$ ./configure
$ make gcc
$ make gcc-dict; cd dict; make utf-8; cd ..    # 辞書のビルド
# make gcc-install
```

これで /usr/local/bin に cmigemo、/usr/local/share/migemo に辞書、
/usr/local/lib に libmigemo.so がインストールされます。

次は PyMigemo を導入します。

```text
$ wget http://www.atzm.org/etc/files/pymigemo/pymigemo-0.3.tar.gz
$ tar xvf pymigemo-0.3.tar.gz
$ cd pymigemo-0.3
$ python setup.py build
# python setup.py install
```

これで無事ビルドされるはずです。

最後に percol を入れます。

```text
$ git clone git://github.com/mooz/percol.git
$ cd percol
# python setup.py install
```

ビルドはこれだけです。あと設定ファイルが必要ですが、無駄に長くなるので
[前のエントリ]({{< relref "2012-04-29-percol-introduction" >}})を見て下さい。


## Windows7 64bit に導入する {#windows7-64bit-に導入する}

Windows の環境は以下のようになっています。

-   Windows7 64bit
-   [Cygwin 1.7.11](http://www.cygwin.com/)
-   python 2.6.7
-   zsh 4.3.12

基本的に Cygwin の使用を前提としています。私は Cygwin のもともと全パッケージを
インストールしているので、以下の工程での細かい必要パッケージがわかりません。
cmigemo も Windows用 dll ではなく、ソースからビルドします。

まずは cmigemo ですがすこし make のコマンドが変わるだけです。先程と同様に
src/wordbuf.c の修正は必要です。

```text
$ wget http://cmigemo.googlecode.com/files/cmigemo-default-src-20110227.zip
$ unzip cmigemo-default-src-20110227.zip
$ cd cmigemo-default-src
$ edit src/wordbuf.c
$ ./configure
$ make cyg
$ make cyg-dict; cd dict; make utf-8; cd ..    # 辞書のビルド
# make cyg-install
```

これで /usr/local/bin に cmigemo と cygmigemo1.dll、/usr/local/share/migemo に
辞書、/usr/local/lib に libmigemo.dll.a がインストールされます。

次に PyMigemo をビルドします。先程とライブラリの名前が違うので、そのまま
ではビルドできません。

```text
$ wget http://www.atzm.org/etc/files/pymigemo/pymigemo-0.3.tar.gz
$ tar xvf pymigemo-0.3.tar.gz
$ cd pymigemo-0.3
$ python setup.py build
running build
running build_ext
building 'migemo' extension
（中略）
gcc -shared -Wl,--enable-auto-image-base build/temp.cygwin-1.7.11-i686-2.6/pymigemo.o -L/usr/lib/python2.6/config -lmigemo -lpython2.6 -o build/lib.cygwin-1.7.11-i686-2.6/migemo.dll
/usr/lib/gcc/i686-pc-cygwin/4.5.3/../../../../i686-pc-cygwin/bin/ld: cannot find -lmigemo
collect2: ld returned 1 exit status
error: command 'gcc' failed with exit status 1
```

setup.py を修正すればいいのかもしれませんが修正の仕方わからなかったので、手動
でlibmigemo.dll.a とリンクしてコンパイルします。

```text
$ gcc -shared -Wl,--enable-auto-image-base build/temp.cygwin-1.7.11-i686-2.6/pymigemo.o \
  -L/usr/lib/python2.6/config -L/usr/local/lib -lmigemo.dll -lpython2.6 \
  -o build/lib.cygwin-1.7.11-i686-2.6/migemo.dll
# python setup.py install
```

これでビルド完了です。

percol は先程と全く同じ工程で出来ますので省きます。


## 使ってみる {#使ってみる}

実際にしぼり込んでみます。

```text
$ ls | percol --match-method migemo
```

{{< figure src="/images/percol_example_migemo.jpg" >}}

という感じに日本語もばっちり絞り込んでくれます。

[前のエントリ]({{< relref "2012-04-29-percol-introduction" >}})で作成した search-document-by-percol はドキュメントを絞り込みます
が、これもばっちり日本語のパスが含まれていても絞り込んでくれるようになりまし
た。1000以上の候補があってもサクサク絞り込んでくれますし、migemo かつ AND 検
索できるようなドキュメント検索ツールは今までにないものだと思います。

zsh の履歴検索もできるかと思ったのですが、日本語を使ったコマンドが .zsh\_history
に正しく保存されず文字化けっぽくなってしまいます。どなたか解決策をご存知の方
に教えていただきたい！


## おわりに {#おわりに}

この機能が実装されて本当に便利すぎて滂沱の涙が出てきました。mooz さん本当にあ
りがとうございます。もう拝まずにいられません。ぜひぜひこのエントリを見た人も使っ
てみてください。