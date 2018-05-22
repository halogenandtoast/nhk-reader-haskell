FROM heroku/heroku:16

RUN apt-get update
RUN apt-get upgrade -y --assume-yes
RUN apt-get install -y --assume-yes g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
RUN apt-get install -y --assume-yes libpq-dev

RUN rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

RUN mkdir -p /opt/nhkreader/src
RUN mkdir -p /opt/nhkreader/bin
WORKDIR /opt/nhkreader/src

ENV PATH "$PATH:/opt/stack/bin:/opt/nhkreader/bin"

COPY ./stack.yaml /opt/nhkreader/src/stack.yaml
RUN stack --no-terminal setup

COPY ./package.yaml /opt/nhkreader/src/package.yaml
RUN stack install yesod-bin --install-ghc
RUN stack --no-terminal test --only-dependencies

COPY . /opt/nhkreader/src
RUN stack --no-terminal build

# Install application binaries to /opt/servant-on-heroku/bin.
RUN stack --no-terminal --local-bin-path /opt/nhkreader/bin install

RUN useradd -ms /bin/bash yesod
RUN chown -R yesod:yesod /opt/nhkreader
USER yesod
ENV PATH "$PATH:/opt/stack/bin:/opt/nhkreader/bin"

WORKDIR /opt/nhkreader/src
CMD /opt/nhkreader/bin/NhkReader
