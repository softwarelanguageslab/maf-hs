FROM haskell:9.4.8-slim

WORKDIR /artifact

RUN mkdir /artifact/maf2-analysis
RUN mkdir /artifact/maf2-domains
RUN mkdir /artifact/maf2-syntax
RUN mkdir /artifact/maf2-scv/

# Install vim and cleanup
RUN apt-get update && apt-get install -y vim \
    && rm -rf /var/lib/apt/lists/*

# Update Hackage index (for a specific date 
# for correct reproducability)
RUN cabal update --index-state=2024-10-01T00:00:00Z

# Only now copy the artifact since this will change 
# often and invalidates the cache.
COPY . /artifact

# Install the analysis package
RUN cabal install maf2-analysis

ENTRYPOINT ["maf-exe"]
