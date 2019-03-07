FROM fpco/stack-build:lts-13.9 as build
WORKDIR /app
COPY . /app
RUN stack build --no-docker --system-ghc
RUN stack install --no-docker --system-ghc

FROM ubuntu:18.04 as app
COPY --from=build /root/.local/bin/sce /usr/local/bin/sce
CMD ["sce"]

FROM jlpz/gotty:1.0.1-ubuntu as webterminal
COPY --from=build /root/.local/bin/sce /usr/local/bin/sce
COPY ./data /data
CMD ["-w", "sce"]
