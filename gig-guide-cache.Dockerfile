FROM haskell:8.6.5@sha256:6ae86033bfa75027fc36a63bb69e7fa41d4e7df6ae4a78baeb77dafc94488d81

WORKDIR /gig-guide

# Use UTF-8 locale:
# Resolved the 'hGetContents: invalid argument (invalid byte sequence)' issue
# when running tests against HTML files containing special characters ('Cafe').
ENV LANG C.UTF-8

COPY *.cabal ./

RUN cabal new-update
RUN cabal new-build --only-dependencies --enable-tests all