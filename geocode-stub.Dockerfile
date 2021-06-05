FROM aringeri/gig-guide-lib

RUN cabal new-install geocode-stub

ENTRYPOINT [ "geocode-stub" ]