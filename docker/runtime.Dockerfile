FROM ghcr.io/aringeri/gig-guide/gig-guide-lib as builder

# An executable to build
ARG EXECUTABLE

# Check that ARG is set up
RUN if [ -z "$EXECUTABLE" ]; then echo "ERROR: Empty $EXECUTABLE"; false; fi

RUN cabal install exe:$EXECUTABLE

FROM debian:bullseye-20250407-slim

RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    ca-certificates \
    curl \
    libgmp10 \
    liblapack3 \
    liblzma5 \
    libpq5 \
    libssl1.1 \
    libyaml-0-2 \
    netbase \
    openssh-client \
    zlib1g \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*


WORKDIR /app

ARG EXECUTABLE
COPY --from=builder /root/.local/bin/$EXECUTABLE ./$EXECUTABLE

# echo into bash script so entrypoint can accept arguments
RUN echo "/app/$EXECUTABLE \$@" > run.sh

ENTRYPOINT [ "bash", "run.sh"]