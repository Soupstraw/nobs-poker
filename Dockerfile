FROM haskell:8.8.3

WORKDIR /opt/nobs-poker

# Install deps
RUN apt update
RUN apt install -y inotify-tools

# Install Elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin

# Build nobs-poker server
WORKDIR /root/nobs-poker
CMD tools/livereload.sh
