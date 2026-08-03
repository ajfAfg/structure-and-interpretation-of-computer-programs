FROM schemers/racket:9-arm

# ca-certificates: raco pkg が HTTPS でパッケージを取得するために必要
# 描画系ライブラリ: sicp-pict(SICP 2.2.4 節の図形言語)が依存する racket/draw のために必要
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        libcairo2 \
        libfontconfig1 \
        libglib2.0-0 \
        libjpeg62-turbo \
        libpango-1.0-0 \
        libpangocairo-1.0-0 \
    && rm -rf /var/lib/apt/lists/*

RUN raco pkg install --auto --batch --no-docs sicp
