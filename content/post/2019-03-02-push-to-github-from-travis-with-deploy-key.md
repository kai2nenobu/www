+++
title = "リポジトリのデプロイキーを使ってTravis CIからGitHubにプッシュする"
author = ["Tsunenobu Kai"]
date = 2019-03-02
tags = ["CI"]
draft = false
+++

CIで静的サイトを生成している場合、生成した静的サイトをリポジトリにコミット＆プッシュしたいことがよくあります。そのためにはCIからリポジトリへのプッシュ権限が必要になります。例えばTravis CIからGitHubにコミットをプッシュする場合は、GitHubのアクセストークンをHTTPSのBasic認証で送信するのがよくあるパターンです。

wip

<!--more-->


## 鍵生成、登録 {#鍵生成-登録}

-   鍵ペア生成
-   デプロイキー登録
-   秘密鍵暗号化
-   秘密鍵を使ってプッシュ

ECDSA384ビットの鍵ペアを生成する。

```console
> ssh-keygen -t ecdsa -b 384 -C travis@travis-ci.org -f www_deploy_key_by_travis
Generating public/private ecdsa key pair.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in www_deploy_key_by_travis.
Your public key has been saved in www_deploy_key_by_travis.pub.
The key fingerprint is:
SHA256:JmxNDpRgGGaJYWUapfvBbnfyCpx4CCPsUOR/vGvAS8U travis@travis-ci.org
The key's randomart image is:
+---[ECDSA 384]---+
|.=BBo...         |
|o=B. ..          |
| oo  .. .        |
|..o. oE=         |
|=o +..* S        |
|=.* *o +         |
| + X =..         |
|  o + =.         |
|     oo.         |
+----[SHA256]-----+
```

秘密鍵 `www_deploy_key_by_travis` と公開鍵 `www_deploy_key_by_travis.pub` が生成される。公開鍵の内容をGitHubのリポジトリのDeploy Keyとしてコピペする。

{{< figure src="/images/writable_key_by_traviskeyboard.png" >}}

`Title` はわかりやすい適当な名前にする。これで公開鍵は用済みなので削除していい。

次に `travis encrypt-file` コマンドで秘密鍵を暗号化する。下記ではDockerコンテナ内で実行する。

```console
> docker run --rm -it -v %CD%:/mnt -w /mnt ruby bash
# gem install travis
# travis login --com
Shell completion not installed. Would you like to install it now? |y| n
We need your GitHub login to identify you.
This information will not be sent to Travis CI, only to api.github.com.
The password will not be displayed.

Try running with --github-token or --auto if you don't want to enter your password anyway.

Username: kai2nenobu
Password for kai2nenobu: ************
Successfully logged in as kai2nenobu!
# travis encrypt-file --com www_deploy_key_by_travis
encrypting www_deploy_key_by_travis for kai2nenobu/www
storing result as www_deploy_key_by_travis.enc
storing secure env variables for decryption

Please add the following to your build script (before_install stage in your .travis.yml, for instance):

    openssl aes-256-cbc -K $encrypted_c546895bd2a4_key -iv $encrypted_c546895bd2a4_iv -in www_deploy_key_by_travis.enc -out www_deploy_key_by_travis -d

Pro Tip: You can add it automatically by running with --add.

Make sure to add www_deploy_key_by_travis.enc to the git repository.
Make sure not to add www_deploy_key_by_travis to the git repository.
Commit all changes to your .travis.yml.
# exit
> rm www_deploy_key_by_travis
> echo www_deploy_key_by_travis >> .gitignore
> git add www_deploy_key_by_travis.enc .gitignore
> git commit -m "暗号化した秘密鍵を追加する"
```

```sh
## pushにsshを使う
$ git config url."git@github.com".pushInsteadOf "https://github.com/"
$ export GIT_SSH_COMMAND="ssh -i file"
$ openssl aes-256-cbc -K $encrypted_c546895bd2a4_key -iv $encrypted_c546895bd2a4_iv -in www_deploy_key_by_travis.enc -out www_deploy_key_by_travis -d
```


## 参考URL {#参考url}

-   [Encrypting Files - Travis CI](https://docs.travis-ci.com/user/encrypting-files/)
-   [Travis CIからgh-pagesへデプロイする設定 via SSH/git push | Web Scratch](https://efcl.info/2016/09/27/deploy-from-travis-ci-to-gh-pages/)
-   [Git: force to use SSH URL instead of HTTPS for github.com](https://discuss.bitrise.io/t/git-force-to-use-ssh-url-instead-of-https-for-github-com/4384)