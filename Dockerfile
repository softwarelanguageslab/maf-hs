FROM haskell:9.4.8-slim

COPY . /artifact
WORKDIR /artifact

RUN cabal update && cabal install maf2-analysis

ENTRYPOINT ["maf-exe"]
