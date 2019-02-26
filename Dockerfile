FROM fpco/stack-build:lts-13.9 as build
WORKDIR /app
COPY . /app
RUN stack build --no-docker --system-ghc
RUN stack install --no-docker --system-ghc
CMD ["sce"]
