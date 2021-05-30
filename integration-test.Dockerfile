FROM aringeri/gig-guide-spec

RUN cabal new-build integration

ENTRYPOINT [ "cabal", "new-test", "integration" ]