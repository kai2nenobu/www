+++
title = "光回線を解約して WiMAX に一本化。URoad-Home は優秀"
author = ["Tsunenobu Kai"]
date = 2012-09-09
tags = ["network"]
draft = false
+++

いままで[auひかり](http://www.auhikari.jp/)の光回線を契約していたが、現在は家の回線と外で使うための回線も
含めて WiMAX に一本化しました。その経緯と WiMAX の使い勝手を紹介しようと思いま
す。
<!--more-->


## まず Mobile Cube を購入する {#まず-mobile-cube-を購入する}

1年ほど前に iPod touch を購入したので、それを屋外でも通信できるように WiMAX
を利用したいと思い始めました。[価格.com](http://kakaku.com/) のプロバイダ料金をチェックした結果、一
番安かった [DTI](http://dream.jp/) と契約して WiMAX 端末の [Mobile Cube](http://nwcs.co.jp/product/mobilecube/) を購入しました。といっても
機器料金は0円で、回線料金は3,880円/月（2年縛り）です。

Mobile Cube の見た目はこんな感じです。

{{< figure src="http://nwcs.co.jp/product/mobilecube/img/name.png" alt="Mobile Cube の外観図" title="Mobile Cube" width="640" link="http://nwcs.co.jp/product/mobilecube/index.html" >}}

-   重さ89g
-   連続通信時間10時間
-   同時接続数8台

といった性能で、小さくて電池がもついい端末だと思います。特に通信時間は（当然通
信量にもよりますが）公称されている時間程度は実際にもっているので、一日の途中で
充電する必要がなく安心です。持ち運び用に専用ケースがついているのもGoodです。

通信速度の方は屋外で使う分には十分な1Mbpsほどでていたので、満足できるものでし
た。また自宅内で使っても中程度の電波強度だったので、ウェブブラウジング程度では
特に光回線との違いも感じられないぐらいの快適さでした。

問題なのは料金で、この時点では光回線と併用していたので

-   光回線: 6,000円程/月
-   WiMAX 回線: 3,880円/月

で月1万円というのは少々高すぎると感じていました。


## 自宅内も WiMAX 回線を使用する {#自宅内も-wimax-回線を使用する}

さっさと光回線を解約してしまえば料金は安くなるのですが、このウェブサーバは自宅
サーバなので自分が自宅にいない間の通信回線が必要です。といってもそれだけのため
に光回線は仰々しすぎます。2012年7月に光回線の2年縛りが満期になったので、光回線
の代わりに新たに WiMAX 回線を契約することにしました。

自宅内で使うので WiMAX 端末に有線のポートがあるものを探していました（自宅サー
バは無線 LAN 通信不可）。最初は充電機能も兼ねるクレードルがついている端末にし
ようかと思っていたのですが、[URoad-Home](http://www.shinseicorp.com/wimax/uroad-home/index.shtml) という非常に有能な端末が発売されていま
した。

{{< figure src="http://www.shinseicorp.com/wimax/uroad-home/img/gallery%5Fphoto02.jpg" alt="URoad-Home の外観図" title="URoad-Home" width="640" link="http://www.shinseicorp.com/wimax/uroad-home/gallery.shtml" >}}

URoad-Home ははじめから自宅内で利用するように想定された非携帯用の WiMAX 端末で、
有線 LAN ポートが2つあるのが今回の用途にぴったりです。この端末の特徴は

-   100BASE 有線 LAN ポートが2つ
-   SSID は2つあり、1SSID につき5台同時接続可能
-   WiMAX ハイパワーなる機能がついており、電波がつながりやすいらしい

といった感じです。

この端末を販売しているプロバイダは少ないですが、私は本家 [UQ WiMAX](http://www.uqwimax.jp/) で1年縛りの
UQ Flat 年間パスポート（3,880円/月）に契約することにしました。肝心の通信速度の
ほうですが、Mobile Cube とあまり変わらず1Mbpsほど出ていてブラウジングでの違和
感は特にありません。また有線接続のためか通信が安定しているように感じます。ため
しにネットゲームもしてみましたが、あまりグラフィック性能を必要としない2Dゲーム
だったためかラグも殆ど感じず快適でした。とはいえ当然有線より遅延は大きいため、
グラフィック性能が求められるFPSなどでは気になるという[レポート](http://www.4gamer.net/games/032/G003289/20120328097/)もあるようです。

また固定回線ではない WiMAX でサーバの運用ができるかが気になっていましたが、ど
こからこのウェブサーバにアクセスしても全く以前と変わらない感触でアクセスするこ
とができました。所詮ウェブサーバだけで通信量は小さくアクセス数も少ないですから、
全然固定回線とかわらず運用できるようで拍子抜けしました。

これで料金の方は

-   Mobile Cube (DTI): 3,880円/月
-   URoad-Home (UQ-WiMAX): 3,880円/月

となり、少し安くなりました。


## 1つ WiMAX 回線を解約し、1回線2機器で運用する {#1つ-wimax-回線を解約し-1回線2機器で運用する}

UQ-WiMAX と契約してからふと思いました。

**\*\*「WiMAX 2回線って無駄じゃね？」**

もともと iPod を屋外で使うために契約した Mobile Cube ですが、思ったより屋外で
通信する頻度も少ないので Mobile Cube だけのために月3,880円払うのが勿体無く感じ
てきました。ここで [WiMAX 機器追加オプション](http://www.uqwimax.jp/service/price/option04.html)というサービスが登場します。既に契
約している回線に対して WiMAX 端末を追加することができます。つまり、今
URoad-Home で使用している回線に Mobile Cube を追加すれば、どちらの端末からの通
信でも1つの回線を介して利用できるようになります。しかし回線が1つなのは変わらな
いので、両方の端末同時に通信することはできません。

いままで機器追加って何に使うんだ、と思ってましたがこういう時のためにあるんだと
得心がいきました。というわけで DTI の契約を途中で解約することにしました。2年縛
り中なので違約金9,600円が発生しましたが、だらだらと続けるよりは安上がりです。
UQ-WiMAX で Mobile Cube を機器追加すると200円/月かかります。

機器追加をして気になるのは2端末での同時通信ができないというところです。しかし
実際に使ってみるとほとんど問題がありませんでした。試しに Mobile Cube を介して
iPod でウェブブラウジングしながら別のマシンで自宅サーバにアクセスしてみました
が、ほとんど通信が途絶しているようには感じられませんでした。ウェブブラウジング
は通信が断続的なので影響がないように見えるのかもしれません。

これで月々の料金は

-   URoad-Home (UQ-WiMAX): 3,880円/月
-   Mobile-Cube（機器追加）: 200円/月

となりました。最初の光回線 + WiMAX 回線とは雲泥の差です。


## まとめ {#まとめ}

光回線と比べた時、WiMAX のメリットや1回線2機器によるメリットは以下の様な点です。

-   なにより安い。固定回線 + モバイル通信回線というのは高くならざるをえない。ボッ
    タクリのパケホーダイなどの代わりに利用すれば効果は抜群です。
-   通信速度はそれなりに早い。完全に光回線と同じというわけには行きませんが、日
    頃のネットワーク通信の95％は光回線と変わりません。
-   必要な機器が少ない。光回線の場合、終端装置やホームゲートウェイなど必要な機器
    が多く、配線が複雑になったりコンセントがタコ足になりがちです。URoad-Home な
    らAC電源1つだけでOK。
-   工事が要らない。自宅への回線の引き込みなどがいらないので、無駄な金をとられる
    こともなく手間がかからない。また引越しの際は URoad-Home を引越し先に持ってい
    けばいいだけなので引越しが楽。これは借家ぐらしには非常に大きなメリットだと思
    う。
-   意外とサーバ運用も出来る。サーバ運用は固定回線がないといけない、となんとなく
    思ってましたがそんなことはなかった。ウェブサーバは以前と全く同じように動いて
    います。遅延があると困るようなサービスでなければ、大概大丈夫なのではないかと
    思います。
-   2機器の同時通信不可はほとんど気にならない。自分のサーバはアクセス数がとても
    少ないので、Mobile Cube と URoad-Home の通信が同時に起こることがほぼないよ
    うです。

なんか書いてて、「業者かっ」と自分で感じてきました。

逆に光回線ではなくなったデメリットは以下のような点。

-   LAN 内の通信はとても遅くなる。URoad-Home の有線 LAN ポートは100BASE
    (100Mbps) なので、光回線で1000BASEを使ってる時と比べると10分の1ぐらいになり
    ました。頻繁に LAN 内でやり取りしている人は気になるかもしれません。
-   遅延が気になる時もある。やはり固定回線と比べると遅延が大きくなります。ssh で
    外のサーバにログインして操作すると若干のもたつきを感じ、光回線の時より少し快
    適さが失われています。といってもそんなに大きな遅延ではない（ping が100msかか
    るくらい）ので操作はちゃんと出来ます。
-   URoad-Home の（無線での）同時接続数が5台しかない。ちょっと大きな自宅内 LAN
    を構成しようと思うと、5台は非常に少ないです。
-   URoad-Home のルータ機能はそれなり。基本的な機能はありますが、本格的なルータ
    よりはだいぶしょぼいです。ポートフォワーディングの設定が16件しか保持できな
    かったり、IP、MAC、ポートによるフィルタリングの設定が一緒くたで16件しか保持
    できなかったりします。今はまだ足りていますが、運用するサーバを増やすと足り
    なくなるかもしれない。

個人的には総じてメリットのほうが上回っており、回線変更してよかったと思います。
今考えると光回線の帯域のほとんどが使えてなかったのだと感じました。もはや光回線
は戸建のファミリー層にしか必要ないのではないでしょうか。単身者には圧倒的に
WiMAX 推しですね。