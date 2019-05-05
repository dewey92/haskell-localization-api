FROM haskell:8.6.3

WORKDIR /opt/server

# Install mysql_config required by persistent-mysql
RUN apt-get update && \
    apt-get install -y apt-utils && \
    apt-get install -y mysql-server && \
    apt-get install -y default-libmysqlclient-dev

# RUN cabal update

# Add just the .cabal file to capture dependencies
# COPY ./api.cabal /opt/server/api.cabal
COPY ./package.yaml /opt/server/package.yaml
COPY ./stack.yaml /opt/server/stack.yaml

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack build --resolver=lts-13.0 --only-dependencies -j1

COPY . /opt/server

# or set to --resolver=ghc-8.6.3
RUN stack build --resolver=lts-13.0 -j1
#--no-haddock-deps --no-haddock-hyperlink-source -j4
