ARG resolver=lts-16.20
FROM fpco/stack-build:${resolver} AS build
WORKDIR /opt/monadick
COPY ./stack.yaml ./package.yaml ./
COPY ./lib/ ./lib/
RUN stack build --only-dependencies
COPY ./private ./private
COPY setup.sql .
COPY ./app/ ./app/
RUN stack install

FROM ubuntu:18.04 AS release
RUN apt-get update \
 && apt-get install -y \
    postgresql-client \
    libpq-dev
WORKDIR /opt/monadick
COPY --from=build /root/.local/bin/bot ./
CMD ["./bot"]
