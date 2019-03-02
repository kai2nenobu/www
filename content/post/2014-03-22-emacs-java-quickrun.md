+++
title = "Java8をemacs-quickrunでコンパイルする"
author = ["Tsunenobu Kai"]
date = 2014-03-22
tags = ["Emacs", "Java"]
draft = false
+++

EmacsのquickrunでJavaを動かすというニッチな人向けです。

<!--more-->


## Java8を入れてみた {#java8を入れてみた}

[Java SE 8](https://blogs.oracle.com/java/entry/java%5Fse%5F8%5Fis%5Fnow)が先日リリースされたので試しに使ってみることにしました。普段のコンパ
イラはJava7のままにしたかったので、Java8の `javac` と `java` は `javac8` と
`java8` というシンボリックリンクを作って利用することにしました。

これをEmacsの[quickrun](https://github.com/syohex/emacs-quickrun)でコンパイル＆実行したいので、マニュアル通り

```emacs-lisp
(quickrun-add-command  "java8"
		       '((:command . "java8")
			 (:compile-only . "javac8 -Werror %o %s")
			 (:exec    . ("javac8 %o %s" "%c %N %a"))
			 (:cmdopt . "-encoding UTF-8")
			 (:remove  . ("%n.class"))
			 (:description . "Compile Java8 file and execute")))
```

として、Java8用の "java8" というコマンドを追加しました。

これでquickrunを実行してみると

```text
qr_65243Cj.java:4: エラー: クラスHogeはpublicであり、ファイルHoge.javaで宣言する必要があります
public class Hoge {
       ^
エラー1個
```

とか怒られました。そういえばquickrunは一時ファイルを作ってそれをコンパイルする
はずなので、ファイル名とクラス名が一致しなくて怒られる。

でも既存のJavaであればquickrunを利用できるはずだけどな？ と思ってquickrunのソー
スを読むとJavaとGoだけは一時ファイルを利用しないらしい。

```emacs-lisp
(defsubst quickrun/use-tempfile-p (cmd-key)
  (not (or (member cmd-key '("java" "go/go")) quickrun/compile-only-flag)))
```

なのでこの関数をアドバイスすることにした。

```emacs-lisp
(defadvice quickrun/use-tempfile-p (after java8 activate)
  "Java8でも一時ファイルを作らないようアドバイス"
  (when (string= (ad-get-arg 0) "java8")
    (setq ad-return-value nil)))
```

これで無事コンパイル＆実行できました。無理やりだけどいいことにしよう。