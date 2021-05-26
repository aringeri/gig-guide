FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81

WORKDIR /beat-web-stub

RUN cabal update

COPY *.cabal ./

RUN cabal new-install --only-dependencies

COPY *.hs ./
COPY src/ ./src
COPY static ./static

RUN cabal new-install beat-web-stub

ENTRYPOINT [ "beat-web-stub" ]