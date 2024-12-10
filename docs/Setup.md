# Setup Guide

This guide provides instructions for setting up the development environment for the futr nostr client on different operating systems.

## Docker

The easiest way to get started is using the pre-built Docker image.

To build the project, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev build
```

To run the tests, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev test
```

To build the Flatpak, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev flatpak
```

To enter the shell of the container, run the following command:

```bash
docker run -it --rm -v $(pwd):/app prolic/futr-dev shell
```

### Build the docker image

```bash
docker buildx build . -t prolic/futr-dev
```

## Linux (Ubuntu / Linux Mint)

For all other Linux distros, you can use the Docker image or follow the Ubuntu / Linux Mint instructions below.
You might need to adjust the package names for your distro.

### GHCUp

You can grab GHCUp from the [GHCUp website](https://www.haskell.org/ghcup/).

```bash
sudo apt-get install build-essential zlib1g-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.6
ghcup install cabal 3.10.3.0
ghcup set ghc 9.6.6
ghcup set cabal 3.10.3.0
```

### Qt5 & LMDB

```bash
sudo apt-get install qtdeclarative5-dev \
                     libqt5quick5 \
                     qttools5-dev-tools \
                     qtbase5-dev \
                     qt5-image-formats-plugins \
                     liblmdb-dev
```

### secp256k1 (from source)

```bash
sudo apt-get install autoconf autogen automake libtool
git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout v0.5.1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental --enable-module-recovery && \
    make && \
    make install && \
    cd ..
```


### Building

```bash
cabal build
```

### Building Flatpak

```bash
cabal build
./build-flatpak.sh
```

## MacOS

@todo

## Windows

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
ghcup install ghc 9.6.6
ghcup install cabal 3.10.3.0
ghcup set ghc 9.6.6
ghcup set cabal 3.10.3.0
```

@todo

## Profiling

Install ghc-prof-flamegraph if you haven't already:

```bash
cabal install ghc-prof-flamegraph
```

Build and run the program with profiling:

```bash
cabal build --enable-profiling --ghc-options="-fprof-auto -rtsopts"
LC_ALL=C cabal run --enable-profiling futr -- +RTS -p
```

Watch the output:

```bash
less futr.prof
```

Generate the flamegraph:

```bash
ghc-prof-flamegraph futr.prof
```

Open the generated file in your browser:

```bash
xdg-open futr.svg
```
