FROM ghcr.io/aringeri/gig-guide/gig-guide-cache

COPY lib/ ./lib
COPY test/ ./test/
COPY executables ./executables
COPY LICENSE ./LICENSE

RUN cabal install --lib library:gig-guide