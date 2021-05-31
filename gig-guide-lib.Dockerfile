FROM aringeri/gig-guide-cache

COPY LICENSE CHANGELOG.md ./
COPY src/ ./src
COPY executables ./executables
COPY integration-test/ ./integration-test
COPY test/ ./test

RUN cabal new-install lib