+++
title = "percol の追加機能いろいろ"
author = ["Tsunenobu Kai"]
date = 2012-11-21
tags = ["percol", "zsh", "tool"]
draft = false
+++

[percol](https://github.com/mooz/percol) が粛々とアップデートされているので、追加機能をいろいろ紹介します。ほと
んどのことが README.md に書いてあるので、詳しくはそちらを。

<!--more-->


## percol 起動中のトグル操作 {#percol-起動中のトグル操作}

`--match-method` オプションでマッチメソッドを変更できますが、percol が起動中に
もマッチメソッドが変更できるようになりました。また大文字小文字を考慮するかも切
り替えられるようになりました。

`toggle_finder()` というコマンドを使うと、現在のマッチメソッドと指定したマッチ
メソッドをトグルできます。 `toggle_case_sensitive()` で大文字小文字の考慮をト
グルです。README に書いてある ~/.percol.d/rc.py の設定例は以下。

```python
from percol.finder import FinderMultiQueryMigemo, FinderMultiQueryRegex
percol.import_keymap({
    "M-c" : lambda percol: percol.command.toggle_case_sensitive(),
    "M-m" : lambda percol: percol.command.toggle_finder(FinderMultiQueryMigemo),
    "M-r" : lambda percol: percol.command.toggle_finder(FinderMultiQueryRegex)
})
```

自分は regex <-> string のトグルと、migemo <-> string のトグルをして欲しかった
ので、自分で `toggle_two_finders()` というコマンドを作りました。
percol/command.py に `toggle_finder()` を真似して

を追記し、percol を再インストールして rc.py を以下のように設定しています。

```python
from percol.finder import FinderMultiQueryString, FinderMultiQueryMigemo, FinderMultiQueryRegex
percol.import_keymap({
    "M-c" : lambda percol: percol.command.toggle_case_sensitive(),
    "M-m" : lambda percol: percol.command.toggle_two_finders(FinderMultiQueryMigemo, FinderMultiQueryString),
    "M-r" : lambda percol: percol.command.toggle_two_finders(FinderMultiQueryRegex, FinderMultiQueryString)
})
```

これでだいぶ Emacs ライクな操作性になりました。migemo のトグルは C-e かと思っ
てましたが、最近の migemo.el は M-m になってるらしいので、M-m にした。


## プロンプトのカスタマイズ {#プロンプトのカスタマイズ}

プロンプト文字がカスタマイズできるようになりました。多分もとからできたんだと
思いますが、README に新たに追加されたので紹介。

`PROMPT` が左のプロンプト、 `RPROMPT` が右のプロンプトを表しています。クラス内
の変数に応じてプロンプトを変更したり、自分独自の format specifier を定義したり
できます。自分の設定はこのようになっています。README の設定とほぼ同じです。

```python
# Change PROMPT in response to the status of case sensitivity
percol.view.__class__.PROMPT = property(
    lambda self:
    ur"<bold><cyan>QUERY </cyan>[a]:</bold> %q" if percol.model.finder.case_insensitive
    else ur"<bold><yellow>QUERY </yellow>[A]:</bold> %q"
)
# Display finder name in RPROMPT
percol.view.prompt_replacees["F"] = lambda self, **args: self.model.finder.get_name()
percol.view.RPROMPT = ur"\<%F\> (%i/%I) [%n/%N]"
```

この設定でプロンプトはこうなります。

{{< figure src="/images/percol_prompt_example.png" >}}

`[a]` の部分で大文字小文字の考慮が、~<string>~ の部分で現在のマッチメソッドが
わかります。これでがんがんトグルできます。他にも文字色や背景色の変更や装飾もで
きるので、自分好みにカスタマイズしましょう。


## 日本語を含む履歴検索 {#日本語を含む履歴検索}

percol を一番利用しているのが zsh の履歴検索なんですが、日本語が文字化けしてし
まうのが難点でした。最近 history コマンドをつかうと、ちゃんと日本語が含まれた
コマンドも参照できることに気づきました。

そして percol の README もいつの間にか history コマンドを使うようになってまし
た。なのでそのまま引用します。

```sh
function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
	local tac
	exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
	BUFFER=$(history -n 1 | eval $tac | percol --query "$LBUFFER")
	CURSOR=$#BUFFER         # move cursor
	zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi
```

これを .zshrc に追加して、履歴検索するとこうなります。

{{< figure src="/images/percol_history_search_japanese.png" >}}

ばっちり日本語も表示できています。これで履歴検索は完璧ですね。


## その他もろもろ {#その他もろもろ}

あとは自分があまり理解してなかったり、試してないのでさらっと。

-   PyPI からインストールできるようになりました。PyPI は python のパッケージ管理
    システムなんですよね？
-   Lazy Array での検索が可能になりました。。Lazy Array がよくわかってないですが、
    全部の候補を検索してから表示するのではなく、候補を検索したはしから順々に表示
    されるってこといいんですかね？ 候補数が正確に表示されない代わりに、パフォー
    マンスの向上が狙えるようです。デフォルトで有効になるので、無効にしたい場合は
    `--eager` オプションをつけましょう。


## おわりに {#おわりに}

少しずつ percol が便利になってます。あとは percol のクエリを履歴に保存しておく
機能があると非常に便利そう。んで `M-p` 、 `M-n` で参照できると。熟練の
pythonista なら実装してくれるはず…|дﾟ)ﾁﾗｯﾁﾗｯ

もっとユーザーが増えて、もっとハックされていくといいですね。みんなでもっと
percol を使いましょう！