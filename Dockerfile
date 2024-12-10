# Use Ubuntu 20.04 LTS as the base image
FROM ubuntu:20.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.ghcup/bin:${PATH}"

# Install necessary packages
RUN apt-get update && apt-get install -y \
    curl \
    git \
    autoconf \
    autogen \
    automake \
    libtool \
    pkg-config \
    qtdeclarative5-dev \
    libqt5quick5 \
    libgflags-dev \
    libsnappy-dev \
    zlib1g-dev \
    zlib1g-dev \
    liblz4-dev \
    libzstd-dev \
    qt5-image-formats-plugins \
    qttools5-dev-tools \
    qtbase5-dev \
    wget \
    sudo \
    build-essential \
    libgmp-dev \
    libffi-dev \
    libncurses5-dev \
    flatpak \
    flatpak-builder \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install specific versions of GHC and Cabal
RUN ghcup install ghc 9.6.6 && ghcup set ghc 9.6.6
RUN ghcup install cabal 3.10.3.0 && ghcup set cabal 3.10.3.0

# Install secp256k1 library
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout v0.5.1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd .. && \
    rm -rf secp256k1

# Set library path
RUN echo "/usr/local/lib" | tee /etc/ld.so.conf.d/local.conf && ldconfig


# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Make the entrypoint script executable
RUN chmod +x entrypoint.sh

# Set the entrypoint
ENTRYPOINT ["./entrypoint.sh"]

# Default command
CMD ["shell"]
