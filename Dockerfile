FROM fpco/stack-build:lts-13.9 as build
WORKDIR /app
COPY . /app
RUN stack build --system-ghc
RUN stack install --system-ghc
CMD ["sce"]
