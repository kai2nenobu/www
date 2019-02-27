+++
title = "キーボード配置を変更する。変態的に。Ubuntu 編"
author = ["Tsunenobu Kai"]
date = 2011-04-17
tags = ["Ubuntu", "mayu", "Emacs"]
draft = false
+++

[前回の記事]({{< relref "2011-04-07-keyboard-config-windows" >}})では Windows 上でどのようにキーボードの配置をカスタマイズしているかを紹介しました．さて，今度は Ubuntu 上で同じキーボードカスタマイズする方法を紹介します．

Windows ではキーボード配置を変更するのに，のどかというアプリケーションを使いましたが，Ubuntu では窓使いの憂鬱を Linux に移植した mayu を使用することにします．


## mayu {#mayu}

窓使いの憂鬱はもともと，UNIX 系から Windows に転職した際に，UNIX 系の操作体系を再現するためのアプリケーションだったはずなのに，今度は Linux 系への逆移植がされて mayu というアプリケーションが開発されるという面白い状態になっています．設定の仕方などは窓使いの憂鬱，のどかとかわらないのがいいですね．

mayu の導入などは他サイトにもあるので，詳しくは書きません．

-   [Linux/Ubuntu/窓使いの憂鬱をインストール - 俺の基地](http://yakinikunotare.boo.jp/orebase/index.php?cmd=read&page=Linux%2FUbuntu%2F%C1%EB%BB%C8%A4%A4%A4%CE%CD%AB%DD%B5%A4%F2%A5%A4%A5%F3%A5%B9%A5%C8%A1%BC%A5%EB)
-   [Ubuntu で窓使いの憂鬱使う方法 - 地獄の猫日記](http://d.hatena.ne.jp/nokturnalmortum/20090227/1235742723)

などを参考にどうぞ．詳しい説明は前回やってしまったので，さっさと私の mayu の設定ファイル (.mayu) の内容を示します．

```text
### .mayu_ubuntu
include "109.mayu" # 109 キーボード設定

keymap Global
## 左コントロールとCapsLockの入れ替え
## CapsLock を Ctrl にして，CapsLock の存在を消し去ることにした
mod Control += Eisuu
key *Eisuu = *LeftControl
#mod Control -= LeftControl
#key LeftControl = Eisuu

## 無変換をAltにする
mod Alt += !!Muhenkan
#key *Muhenkan = *RightAlt
## 単独で押したら ESC にする(one shot modifier)
key ~R-*M-Muhenkan = Escape
key R-*M-Muhenkan = &Ignore

## 変換をCrtlにする．
mod Ctrl += !!Henkan
#key *Henkan = *RightControl
## 単独で押したら ENTER にする(one shot modifier)
key ~R-*C-Henkan = Enter
key R-*C-Henkan = &Ignore

#スペースをshiftとして使用(SandS)
mod Shift += !!!Space
def option delay-of !!! = 2

keymap  KeymapDefault = &Default
```

以上の設定で，前回説明した caps Lock を ctrl にすることと，shift, ctrl, alt (SandS, one shot modifier) を親指で押せる位置に変更する設定ができます．記述は殆ど変わりません．キーの名前が日本語からアルファベットになっているくらいです．


## super, hyper の設定 {#super-hyper-の設定}

Windows の時と同様に super キーや，hyper キーは mayu では設定できません．そこで，Linux でキーボード配置を変更するプログラムとして定番な xmodmap を使います．xmodmap は割と mayu に似た記法でキーボード配置を変更します（というか窓使いの憂鬱のほうが参考にしたのでしょうね）．

では，カタカナひらがなキーを super キーにします．xmodmap の設定ファイル.Xmodmap（多分ファイル名はなんでもいいはずですが）に以下の設定を書きます．

```text
!! カタカナひらがなをHyper keyにする
keysym Hiragana_Katakana = Hyper_L
remove mod4 = Hyper_L
add mod3 = Hyper_L
```

実際にカタカナひらがなキーに hyper を割り当てているのは2行目だけで，他の行はあんまり重要ではありません．この設定ファイルを

```text
$ xmodmap .Xmodmap
```

で設定ファイルを読み込むと，設定が有効になります．xmodmap コマンドを単独で使うと現在どのキーに修飾キーが割り当てられているかわかります．

```text
$ xmodmap
xmodmap:  up to 3 keys per modifier, (keycodes in parentheses):

shift       Shift_L (0x32),  Shift_R (0x3e)
lock        Eisu_toggle (0x42)
control     Control_L (0x25),  Control_R (0x69)
mod1        Alt_L (0x40),  Alt_R (0x6c),  Meta_L (0xcd)
mod2        Num_Lock (0x4d)
mod3        Hyper_L (0x65),  Hyper_L (0xcf)
mod4        Super_L (0x85),  Super_R (0x86),  Super_L (0xce)
mod5        ISO_Level3_Shift (0x5c),  Mode_switch (0xcb)
```

mod3 に hyper が，mod4 に super が割り当てられているのがわかると思います．Ubuntu ではもともと windows キーに super が割り当てられていたので，xmodmapで設定するまでもありませんでした．この辺は，ディストリビューションやバージョンによって変わってくるかもしれません．


## おわりに {#おわりに}

これで，Windows と Ubuntu でほぼ同じキー配置を使うことができます（厳密に言えばキーボードの入力がどこでトラップされるのかによって細かい動きが変わってくるようですが）．これで Emacs の職業病，左手の小指痛を全く感じずに快適に使うことができます！