# structure-and-interpretation-of-computer-programs

## 開発環境

[schemers/racket](https://hub.docker.com/r/schemers/racket) イメージをベースにした Docker 開発環境を利用する(バージョンは `Dockerfile` の `FROM` で固定)。[sicp パッケージ](https://docs.racket-lang.org/sicp-manual/)がインストール済みで、`#lang sicp` と図形言語(`sicp-pict`)が使える。リポジトリルートがコンテナの `/workspace` にマウントされる。

初回実行時はイメージのビルドが自動で走る。`Dockerfile` を変更した場合は `docker compose build` で再ビルドする。

```sh
# コンテナ内でシェルを起動
docker compose run --rm racket

# REPL を起動
docker compose run --rm racket racket

# ファイルを実行
docker compose run --rm racket racket path/to/file.rkt
```
