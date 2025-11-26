FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-22.04

# Install Haskell (GHC, Cabal, Stack)
RUN apt-get update && \
    apt-get install -y haskell-platform curl && \
    curl -sSL https://get.haskellstack.org/ | sh
