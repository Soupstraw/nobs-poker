FROM haskell:8.8.3

WORKDIR /opt/nobs-poker

# Install Elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin

# Build nobs-poker client
COPY client client
WORKDIR /opt/nobs-poker/client
RUN elm make src/Main.elm --output=elm.js

# Build nobs-poker server
WORKDIR /opt/nobs-poker
COPY src src
COPY app app
COPY test test
COPY package.yaml stack.yaml README.md ChangeLog.md ./
RUN stack install

FROM alpine:latest
WORKDIR /root
RUN apk add libc6-compat gmp
COPY --from=0 /root/.local/bin/nobs-server .
COPY --from=0 /opt/nobs-poker/client client
CMD ./nobs-server
