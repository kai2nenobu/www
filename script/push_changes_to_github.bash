#!/bin/bash

declare -r SCRIPT_NAME="$0"
declare -r GITHUB_DEPLOY_KEY="$(mktemp $HOME/.ssh/XXXXXXX)"
trap 'clean_up' EXIT

clean_up() {
  rm -f "$GITHUB_DEPLOY_KEY" || true
}

### コマンドを実行する前に実行するコマンドを出力する関数
### CIなどで、なにをやってるかをわかりやすくするために使う。
### **秘密情報を含む引数を渡さないこと！**
doing() {
  echo "[${SCRIPT_NAME}]" '$' "$@"
  "$@"
}

### doingの実際には
script_echo() {
  echo "[${SCRIPT_NAME}]" '$' "$@"
}

## 変更があればコミットする
git diff
git add content
doing git -c user.name="Travis CI" -c user.email="travis@travis-ci.org" commit -m "[ci skip] Update documents generated from ${TRAVIS_COMMIT:0:7}"

## デプロイキーを復号する
script_echo openssl aes-256-cbc でデプロイキーを復号する
openssl aes-256-cbc -K $encrypted_c546895bd2a4_key -iv $encrypted_c546895bd2a4_iv -in .travis/www_deploy_key_by_travis.enc -out "$GITHUB_DEPLOY_KEY" -d

## sshを使ってpushするようにURLを変更する
git config url."git@github.com:".pushInsteadOf "https://github.com/"
doing git remote -v

## デプロイキーを使ってpushする
export GIT_SSH_COMMAND="ssh -i ${GITHUB_DEPLOY_KEY}"
doing git push origin "HEAD:${TRAVIS_BRANCH}"
