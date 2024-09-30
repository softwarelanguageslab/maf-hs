FROM haskell:9.4.8-slim

COPY . /artifact
WORKDIR /artifact

RUN echo "nameserver 1.1.1.1" >> /etc/resolv.conf
RUN cabal update && cabal install maf2-analysis

CMD /bin/sh
