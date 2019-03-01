+++
title = "anything 的な絞りこみコマンド percol の紹介"
author = ["Tsunenobu Kai"]
date = 2012-04-29
tags = ["percol", "zsh", "tool"]
draft = false
+++

いつも [KeySnail](https://github.com/mooz/keysnail/wiki/keysnail-japanese) でお世話になっている [mooz](http://d.hatena.ne.jp/mooz/) さんが、percol という超絶便利コマ
ンドを作ってらっしゃいます。このコマンドはとてもライフチェンジングなコマンドな
のですが、あまり Web 上に情報がないので紹介がてら布教してみようと思います。

<!--more-->


## percol のインストール＆使い方 {#percol-のインストール-使い方}

[percol](https://github.com/mooz/percol) は入力の1行を1候補として、部分一致かつ AND 検索で絞り込みし、選択した候
補を出力するコマンドです。端的に言えば Emacs の anything.el のコマンド版です。

インストール方法や基本的な使い方は github リポジトリの README に全部書いてあり
ますが備忘録として書いておきます。

```text
$ git clone git://github.com/mooz/percol.git
$ cd percol
# python setup.py install
```

これでインストールは完了です。インストール場所を変えたければ、setup.py に
`--prefix` オプションをつけます。

percol を動かす前に、percol の設定ファイルが必要です。とりあえず
README からそのままコピーしてきた以下の内容のファイルを、
${HOME}/.percol.d/rc.py に保存します。

```python
# X / _ / X
percol.view.PROMPT  = ur"<bold><yellow>X / _ / X</yellow></bold> %q"

# Emacs like
percol.import_keymap({
    "C-h" : lambda percol: percol.command.delete_backward_char(),
    "C-d" : lambda percol: percol.command.delete_forward_char(),
    "C-k" : lambda percol: percol.command.kill_end_of_line(),
    "C-y" : lambda percol: percol.command.yank(),
    "C-a" : lambda percol: percol.command.beginning_of_line(),
    "C-e" : lambda percol: percol.command.end_of_line(),
    "C-b" : lambda percol: percol.command.backward_char(),
    "C-f" : lambda percol: percol.command.forward_char(),
    "C-n" : lambda percol: percol.command.select_next(),
    "C-p" : lambda percol: percol.command.select_previous(),
    "C-v" : lambda percol: percol.command.select_next_page(),
    "M-v" : lambda percol: percol.command.select_previous_page(),
    "M-<" : lambda percol: percol.command.select_top(),
    "M->" : lambda percol: percol.command.select_bottom(),
    "C-m" : lambda percol: percol.finish(),
    "C-j" : lambda percol: percol.finish(),
    "C-g" : lambda percol: percol.cancel(),
})
```

このファイルでキーバインドやプロンプトを変えることができるようなので、好みに
合わせて変えてみてください。

percol の簡単な使い方は、行で分割されている出力をパイプで percol に入力します。
例えば

```text
$ ls / | percol
```

とすると

{{< figure src="/images/percol_example.jpg" >}}

このようにルートにあるファイルが候補になります。文字を入力して絞り込んだり、
C-n, C-p で移動して１つの候補を選びます。最後に決定 (Enter, C-m, C-j) を押す
とその候補を標準出力します。


## 関数を作る {#関数を作る}

percol は絞り込みするためのコマンドなので、あらかじめある目的の候補群から絞込
みをする関数を作っておくと便利です。典型的な例が README にも書いてある、シェル
の履歴を percol で絞込みする関数です。

まずこの関数を .zshrc に書いておきます。

```sh
function percol_select_history() {
  local tac_cmd
  which gtac &> /dev/null && tac_cmd=gtac || tac_cmd=tac
  BUFFER=$($tac_cmd ~/.zsh_history | sed 's/^: [0-9]*:[0-9]*;//' \
    | percol --match-method regex --query "$LBUFFER")
  CURSOR=$#BUFFER         # move cursor
  zle -R -c               # refresh
}
zle -N percol_select_history
bindkey '^R' percol_select_history
```

この設定をしてコマンドラインで C-r を押すと

{{< figure src="/images/percol_example_history.jpg" >}}

このような画面になり、正規表現部分一致かつ AND 検索で過去のコマンド履歴を絞り込
めます。Enter を押すと現在選択中の行のコマンドがコマンドラインに挿入され、そ
のまま実行するなり、一部改変して実行するなりできます。この関数が便利すぎて非
常に泣けてきます。

他にもいくつか関数を自作してみましたので紹介します。

私はドキュメントファイルは特定のディレクトリにおいてあることがほとんどなので、
ディレクトリ中のドキュメントファイルを絞り込んで開く関数を作っています。

```sh
function search-document-by-percol(){
  DOCUMENT_DIR="\
/path/to/doc/directory1
/path/to/doc/directory2"
  SELECTED_FILE=$(echo $DOCUMENT_DIR | xargs find | \
    grep -E "\.(pdf|txt|odp|odt|ods)$" | percol --match-method regex)
  if [ $? -eq 0 ]; then
    gnome-open $SELECTED_FILE
  fi
}
alias sd='search-document-by-percol'
```

これを .zshrc に書いておきます。この関数を呼び出せば、ドキュメントファイルが列
挙され絞り込むことができます。gnome-open はファイルの拡張子に対応するプログラ
ムを起動するコマンドなので、OS に応じて open なり、cygstart なりに変えてくださ
い。この関数のお陰で（ファイル名やキーワードを覚えてさえいれば）どのファイルに
も5秒程度でアクセスできるようになりました。

次の関数はカレントディレクトリのファイルを絞り込んでプロンプトに挿入します。

```sh
function insert-file-by-percol(){
  LBUFFER=$LBUFFER$(ls -A | percol --match-method regex | tr '\n' ' ' | \
    sed 's/[[:space:]]*$//') # delete trailing space
  zle -R -c
}
zle -N insert-file-by-percol
bindkey '^[c' insert-file-by-percol
```

これを .zshrc に書いておくと、M-c で絞込みを開始します。カレントディレクトリの
ファイルの補完は当然 zsh の TAB でできます。しかし file001～file100 のように同
じ接頭語のファイルが複数あるディレクトリでファイルを補完する場合には、この関数
のほうが有利かもしれません。マークすることにより同時に複数ファイルを挿入できま
す。


## まとめ {#まとめ}

anything 的な絞りこみコマンド percol の紹介をしました。最近 Emacs でも
anything 脳が着々と進み、そろそろ OS レベルで anything 的なインターフェースを
用意してくれないかなー、と思っていた矢先 percol を見つけたので大変重宝してい
ます。

percol の以前にも同じようなコマンドとして [zaw](https://github.com/zsh-users/zaw) や [canything](http://filmlang.org/soft/canything) が開発されていまし
た。が、前者は zsh 依存ですし、後者は正規表現や日本語が扱えず少々機能不足です。
その点 percol のいいところは

-   python さえあればどこでも動きます。私は基本的には Ubuntu で使っていますが、
    Cygwin on Winodws7 でも普通に動きます。すばらしい。
-   アクションで拡張可能。絞込み中に TAB を押すとアクションが選択できます。デフォ
    ルトでは、標準出力する、というアクションしかありませんが、python スクリプト
    を自分で書けばいくらでもアクションを定義できるようです。ユーザの発想次第で
    いくらでも便利に拡張できます。

あたりがあげられるかと思います。

逆に悪い点というか、個人的な要望としては以下の様な点があります。

-   percol が起動するのに少々時間がかかります。Ubuntu ではほぼ気になりませんが、
    Cygwin だと0.5秒ほどかかります。許容範囲内ではありますが。
-   一旦マークした後に絞り込み条件を変更するとマークが解除されてしまいます。絞
    り込みを渡り歩きながら複数候補をマークすることができないので、若干不便です。
-   ~~日本語入力はできますが、個人的には migemo れると最高。正規表現との混在はめ
    んどくさいと思うので、--match-method migemo とかあると感涙に咽びなきます。~~
    ばっちり migemo 対応して下さいました。[こちら]({{< relref "2012-05-12-percol-migemo" >}})も御覧ください。

もし percol に興味を持った方は、github の README をとりあえず真似してみて、
履歴検索の威力をためしてみるといいと思います。

最後に mooz さん、とってもすばらしいコマンドを開発していただきありがとうざいま
す。