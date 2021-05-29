FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81 AS cache

WORKDIR /gig-guide

RUN cabal new-update

COPY *.cabal ./

RUN cabal new-install --only-dependencies

FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81 AS builder

WORKDIR /gig-guide

COPY --from=cache /gig-guide/dist-newstyle/ /gig-guide/dist-newstyle/
COPY --from=cache /root/.cabal/ /root/.cabal/

COPY *.cabal ./
COPY src/ ./src
COPY executables ./executables
COPY integration-test/ ./integration-test
COPY test/ ./test

RUN cabal new-build

FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81 AS tester

WORKDIR /gig-guide

COPY --from=builder /gig-guide/dist-newstyle/ /gig-guide/dist-newstyle/
COPY --from=builder /root/.cabal/ /root/.cabal/

COPY *.cabal ./
COPY src/ ./src
COPY executables ./executables
COPY integration-test/ ./integration-test
COPY test/ ./test

RUN cabal new-build integration

ENTRYPOINT [ "cabal", "new-test", "integration" ]