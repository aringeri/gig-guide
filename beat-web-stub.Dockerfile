FROM aringeri/gig-guide-lib

RUN cabal new-install beat-web-stub

ENTRYPOINT [ "beat-web-stub" ]