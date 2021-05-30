FROM aringeri/gig-guide-lib

RUN cabal new-build spec

ENTRYPOINT [ "cabal", "new-test", "spec" "--test-show-details=direct" ]