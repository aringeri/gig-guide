FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81

WORKDIR /gig-guide

RUN cabal new-update

COPY *.cabal LICENSE CHANGELOG.md ./
COPY src/ ./src
COPY executables ./executables
COPY integration-test/ ./integration-test
COPY test/ ./test

RUN cabal new-install --only-dependencies lib webserver scrape-events scrape-venues geocode-venues