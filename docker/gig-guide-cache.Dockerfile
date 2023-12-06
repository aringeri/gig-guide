FROM haskell:9.4.7

WORKDIR /gig-guide

# Use UTF-8 locale:
# Resolved the 'hGetContents: invalid argument (invalid byte sequence)' issue
# when running tests against HTML files containing special characters ('Cafe').
#ENV LANG C.UTF-8

COPY *.cabal ./

RUN cabal update
RUN cabal build --only-dependencies --enable-tests all