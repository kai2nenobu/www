+++
title = "Emacs Lispのテスト、依存性管理、CI"
author = ["Tsunenobu Kai"]
date = 2014-02-23
tags = ["Emacs", "Test"]
draft = false
+++

現在開発中の[guide-key](https://github.com/kbkbkbkb1/guide-key)の機能がそれなりに増えてきたので、そろそろテストを書きた
いなと思いました。そこでERTでユニットテストを書いて、Caskで依存関係を解決して、
Travis CIでCIするところまでできたので紹介します。

<!--more-->


## ERTでユニットテスト {#ertでユニットテスト}

[ERT](http://www.emacswiki.org/emacs/ErtTestLibrary)はEmacs Lisp Regression Testingの略で、Emacs Lispのテスティングツールです。
JUnitなどと同様にユニットテストが書けます。


### ディレクトリ構成 {#ディレクトリ構成}

-   [EmacsLispで最小構成(っぽい)テストをする方法 - プログラムとかののblog](http://d.hatena.ne.jp/pogin/20130617/1371488876)

が丁度良くテストの最小構成を紹介していたので、真似して以下のようなディレクトリ
構成にしました。

```text
guide-key/
├── guide-key.el
└── test/
    └── guide-key-test.el
```

参考にしたリポジトリも、概ねリポジトリ直下に `test` などのテスト専用のディレ
クトリを配置しているのが多かったです。


### テストを書いて実行 {#テストを書いて実行}

テストの書き方の詳細は[マニュアル](http://www.gnu.org/software/emacs/manual/ert.html)に譲りますが、 `ert-deftest` でテストケースを
定義し、 `should` でアサーションすることができます。とりあえず最近追加した関数
のテストを書いてみました。

```emacs-lisp
(require 'ert)
(require 'guide-key)
(eval-when-compile
  (require 'cl))

(ert-deftest guide-key-test/get-highlight-face ()
  "Test of `guide-key/get-highlight-face'"
  (let ((guide-key/highlight-command-regexp
	 '("rectangle"
	   ("register" . font-lock-type-face)
	   ("bookmark" . font-lock-warning-face)
	   ))
	(fixtures
	 '(("Prefix Command" . guide-key/prefix-command-face)
	   ("string-rectangle" . guide-key/highlight-command-face)
	   ("jump-to-register" . font-lock-type-face)
	   ("bookmark-jump" . font-lock-warning-face)
	   ("copy-rectangle-to-register" . guide-key/highlight-command-face)
	   ("<NOTEXIST>" . nil)
	   ))
	actual)
    (loop for (input . expected) in fixtures
	  do
	  (setq actual (guide-key/get-highlight-face input))
	  (should (eq actual expected)))
    ))
```

まだテストの前処理、後処理やテストケースの構造化をする方法がよくわからないの
で、識者の意見がほしいです。

テストの実行するにはEmacs内から `ert` コマンドを実行するか、以下のようにコマン
ドラインからEmacsのバッチを呼び出します。

```text
$ emacs -batch -l test/guide-key-test.el -f ert-run-tests-batch-and-exit
```

しかしguide-keyは[popwin](https://github.com/m2ym/popwin-el)に依存しているため、このコマンドだけではテストが実行で
きません。popwinのあるところに `load-path` を通す必要があります。これを手動で
やるのは大変なので、Caskという依存関係を解決してくれるツールを利用することにし
ました。


## Caskで依存関係管理 {#caskで依存関係管理}

[Cask](https://github.com/cask/cask)はEmacsの依存関係管理ツールです。標準添付のpackage.elもライブラリの依存関
係を考慮して必要ライブラリを一括でダウンロードすることはできます。しかし 個人
の設定のために `~/.emacs.d` にダウンロードする前提なので、ライブラリの開発で
利用するのには向きません。

CaskはRubyでいうGemfileのように、Caskファイルに依存しているライブラリを記述す
ることで、一括ダウンロードや `PATH` 、 `load-path` の調整をしてくれます。


### インストール {#インストール}

```text
$ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
$ export PATH="$HOME/.cask/bin:$PATH"
```

だけです。

ちなみにWindowsで試してみたら、うまくインストール出来ませんでした。Cygwinなの
が悪いのでしょうか。


### 依存関係の解決 {#依存関係の解決}

リポジトリの直下の `Cask` ファイルに、依存しているライブラリを記述します。例え
ばこのような感じです。

```emacs-lisp
(source gnu)
(source melpa)
(source marmalade)

(package-file "guide-key.el")

(development
 (depends-on "ert")
 (depends-on "popwin"))
```

`source` がライブラリを参照する場所、 `package-file` が開発しているライブラリ、
`depends-on` が依存しているライブラリです。詳しくは[Usage](http://cask.github.io/usage/)を見てください。
guide-key自身が依存しているのはpopwinだけですが、Emacs23でテストする際にertが
必要になるのでertも依存ライブラリとしています。

実際に依存ライブラリをダウンロードするには、コマンドラインで `cask` あるいは
`cask install` します。

```text
$ cask install
Contacting host: marmalade-repo.org:80
Saving file /home/kai/.emacs.d/my-lisp/guide-key/.cask/24.3.1/elpa/archives/marmalade/archive-contents...
（中略）
Wrote /home/kai/.emacs.d/my-lisp/guide-key/.cask/24.3.1/elpa/popwin-20130329.435/popwin.elc
Done (Total of 2 files compiled, 1 skipped)
$ ls .cask/24.3.1/elpa/
archives  popwin-20130329.435
```

これで `.cask` ディレクトリが作成され、依存ライブラリがダウンロードされます。

この状態で

```text
$ cask exec command
```

することで、 `.cask` 以下にある依存ライブラリを `PATH` や `load-path` に追加し
た状態で `command` を実行することができます。したがって以下のコマンドでテスト
を実行することができます。

```text
$ cask exec emacs -batch -L . -l test/guide-key-test.el -f ert-run-tests-batch-and-exit
Running 1 tests (2014-02-23 12:56:40+0900)
   passed  1/1  guide-key-test/get-highlight-face

Ran 1 tests, 1 results as expected (2014-02-23 12:56:40+0900)
```

無事テストが成功しました。

Caskを使えばpopwinへの `load-path` を考える必要がないのが楽です。ただしCaskは
リポジトリ直下（guide-key.elがあるディレクトリ）を `load-path` に追加してくれ
ないようなので、 `-L .` で手動で追加しています。あまり美しい方法では無いですね。
参考にしたリポジトリでは、 `test/test-init.el` などのテスト初期化ファイルを作っ
て、そこで開発ライブラリ（guide-key.el）をロードするような構成になっているもの
もありました。


### 環境変数で環境を切り替える {#環境変数で環境を切り替える}

環境変数 `EMACS` を設定することで、Caskで利用するEmacsを切り替えることができま
す。上の `emacs` はバージョンが24でしたが、それとは別にバージョン23の
`emacs23` がインストールされている場合、以下のように `cask` を環境変数を変更し
て実行します。

```text
$ export EMACS=emacs23
$ cask install
Contacting host: marmalade-repo.org:80
Saving file /home/kai/.emacs.d/my-lisp/guide-key/.cask/23.3.1/elpa/archives/marmalade/archive-contents...
（中略）
Wrote /home/kai/.emacs.d/my-lisp/guide-key/.cask/23.3.1/elpa/popwin-20130329.435/popwin.elc
Done (Total of 2 files compiled, 1 skipped)
$ ls .cask/23.3.1/elpa
archives  ert-0  popwin-20130329.435
```

あとは先ほどと同様に

```text
$ cask exec ${EMACS} -batch -L . -l test/guide-key-test.el -f ert-run-tests-batch-and-exit
Running 1 tests (2014-02-23 12:56:40+0900)
   passed  1/1  guide-key-test/get-highlight-face

Ran 1 tests, 1 results as expected (2014-02-23 12:56:40+0900)
```

でテストが回せます。popwinやertへの `load-path` を考える必要がなく、同じコマン
ドなのがいいですね。


## Travis CIでCI {#travis-ciでci}

テストが書けたのでCIできるように[Travis CI](http://docs.travis-ci.com/user/getting-started/)を利用します。


### Makefileで自動化 {#makefileで自動化}

`make` コマンド一発でテストを回すために、Makefileを作ります。

```makefile-gmake
EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	# Fail if byte-compile outpus warnings
	${CASK} exec ${EMACS} -batch -Q -L . -eval \
	"(progn \
	(setq byte-compile-error-on-warn t) \
	(batch-byte-compile))" guide-key.el
test:
	${CASK} exec ${EMACS} -Q -batch -L . -l test/guide-key-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f guide-key.elc

.PHONY: all compile test clean
```

簡単なMakefileですが `make` コマンドでバイトコンパイルせずにテストと、バイトコ
ンパイルしてテストを実行します。バイトコンパイルで警告が出ると失敗させているの
は厳しすぎるかもしれませんが、当面これで行くことにしました。


### Travisの設定 {#travisの設定}

Travisのビルド設定をtravis.ymlに書きます。

```yaml
language: emacs-lisp
env:
  - EMACS=emacs23
  - EMACS=emacs24
  - EMACS=emacs-snapshot
matrix:
  allow_failures:
    - env: EMACS=emacs-snapshot
before_install:
  # Install Emacs
  - sudo add-apt-repository -y ppa:cassou/emacs
  - sudo apt-get update -qq
  - sudo apt-get install -qq $EMACS
  # Install Cask
  - curl -fsSkL --max-time 10 --retry 10 --retry-delay 10
	https://raw.github.com/cask/cask/master/go | python
  - export PATH="$HOME/.cask/bin:$PATH"
  - cask
script:
  make
```

`before_install` で必要なEmacsとCaskをインストールして、テストを回します。テス
ト環境は `emacs23` と `emacs24` と `emacs-snapshot` の3つとし、環境変数
`EMACS` を設定することで自動的に `cask` の動作が変わるようになります。

実際にビルドした結果が以下のようになります。

{{< figure src="/images/guide-key-travis-ci.png" alt="guide-keyのTravis CIでのビルド結果" title="Travis CIへGo" link="https://travis-ci.org/kbkbkbkb1/guide-key" >}}

`emacs-snapshot` がなぜか失敗しているので、やむを得ず `allow_failures` に入れ
てます。


## まとめ {#まとめ}

Emacs Lispのテスト、依存性管理、CIする方法を紹介しました。最終的なディレクトリ
構成は以下のようになりました。

```text
guide-key/
├── .cask/          # 依存ライブラリを格納
│   ├── 23.3.1/      # Emacsのバージョン別に保持
│   ├── 24.3.1/
│   └── ...
├── .travis.yml     # Travis CIの設定
├── Cask            # 依存ライブラリを記述
├── Makefile        # テストの自動化
├── guide-key.el
└── test/
    └── guide-key-test.el
```

あとはテストケースが全然不十分なので、テストケースを充実させていくだけです。
guide-keyは副作用がある関数ばかりなので、テストが書きにくそうです。できるだけ
副作用のない粗結合の構成になるようにリファクタリングしたいと思います。

さらにテストを便利にするためのライブラリとして、[rejeep/ert-runner.el](https://github.com/rejeep/ert-runner.el)や
[ecukes/ecukes](https://github.com/ecukes/ecukes)があります。ert-runnerはJUnitでいうテストスイートのようなもので、
テスト名やタグによって実行するテストケースを制御するライブラリです。ecukesは
cucumberのように振る舞い駆動開発するためのライブラリのようです。

これらもおいおい導入していければと思います。


### 参考にしたリポジトリ {#参考にしたリポジトリ}

-   Caskを作っている[rejeep (Johan Andersson)](https://github.com/rejeep)さんのリポジトリ
    -   [cask/cask](https://github.com/cask/cask)
    -   [ecukes/ecukes](https://github.com/ecukes/ecukes)
    -   [rejeep/ert-runner.el](https://github.com/rejeep/ert-runner.el)
    -   [rejeep/f.el](https://github.com/rejeep/f.el)
-   テストを作る際のテンプレート
    -   [pogin503/emacs-test-sample](https://github.com/pogin503/emacs-test-sample) ERTでテストする最小構成。
    -   [lewang/ert-test-skeleton](https://github.com/lewang/ert-test-skeleton) Travis CIを利用するテンプレート（Caskなし）。
    -   [tkf/emacs-plugin-template](https://github.com/tkf/emacs-plugin-template) CaskとTravis CIを利用したテンプレート。Caskの旧
        名のCartonが使われているが、Caskでもほぼそのまま利用できる。