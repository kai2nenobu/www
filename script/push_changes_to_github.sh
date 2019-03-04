#!/bin/sh

GITHUB_DEPLOY_KEY="$(mktemp -u $HOME/.ssh/XXXXXXX)"
trap 'clean_up' EXIT

clean_up() {
  rm -f "$GITHUB_DEPLOY_KEY" || true
}

## 変更があればコミットする
git diff
git add content
git -c user.name="Travis CI" -c user.email="travis@travis-ci.org" commit -m "[ci skip] Update documents generated from ${TRAVIS_COMMIT:0:7}"

## デプロイキーを復号する
openssl aes-256-cbc -K $encrypted_c546895bd2a4_key -iv $encrypted_c546895bd2a4_iv -in .travis/www_deploy_key_by_travis.enc -out "$GITHUB_DEPLOY_KEY" -d

## sshを使ってpushするようにURLを変更する
git config url."git@github.com:".pushInsteadOf "https://github.com/"
git remote -v

## デプロイキーを使ってpushする
export GIT_SSH_COMMAND="ssh -i ${GITHUB_DEPLOY_KEY}"
git push origin "HEAD:${TRAVIS_BRANCH}" >/dev/null 2>&1
