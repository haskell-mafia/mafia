Installing GHC
--------------

These instructions are for OS/X, but should work equally well on Linux
if you replace `apple-darwin` with the relevant target platform for your
operating system. Use the GHC
[download](https://www.haskell.org/ghc/download) page to locate the
relevant tarball for your system.

In the examples below, we use `$HOME/haskell/ghc-<version>` as the
install destination for GHC. You can however, install GHC wherever you
like.

### Simple (Single GHC Version)

GHC 7.10.2 is the current stable/preferred version of GHC used by
Ambiata projects.

#### Install GHC 7.10.2

```sh
$ wget https://downloads.haskell.org/~ghc/7.10.2/ghc-7.10.2-x86_64-apple-darwin.tar.xz
$ tar xf ghc-7.10.2-x86_64-apple-darwin.tar.xz
$ cd ghc-7.10.2
$ ./configure --prefix=$HOME/haskell/ghc-7.10.2
$ make install
```

#### Add to PATH

Add the following to your `.zshrc`, `.bashrc` or `.bash_profile`:

```sh
export PATH=$HOME/haskell/ghc-7.10.2/bin:$PATH
```

### Advanced (Multiple GHC Versions)

Use these instructions if you want to run multiple versions of GHC side-by-side.

This setup doesn't have GHC on the `PATH` by default, and uses `g` to cycle between available versions.

#### Install GHC 7.8.4

```sh
$ wget https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-apple-darwin.tar.xz
$ tar xf ghc-7.8.4-x86_64-apple-darwin.tar.xz
$ cd ghc-7.8.4
$ ./configure --prefix=$HOME/haskell/ghc-7.8.4
$ make install
```

#### Install GHC 7.10.2

```sh
$ wget https://downloads.haskell.org/~ghc/7.10.2/ghc-7.10.2-x86_64-apple-darwin.tar.xz
$ tar xf ghc-7.10.2-x86_64-apple-darwin.tar.xz
$ cd ghc-7.10.2
$ ./configure --prefix=$HOME/haskell/ghc-7.10.2
$ make install
```

#### Install GHC 8.0.1

```sh
$ wget https://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-x86_64-apple-darwin.tar.xz
$ tar xf ghc-8.0.1-x86_64-apple-darwin.tar.xz
$ cd ghc-8.0.1
$ ./configure --prefix=$HOME/haskell/ghc-8.0.1
$ make install
```

#### Zsh

Add the following to your `.zshrc`:

```zsh
# List available GHC versions
ghc-list-available() {
  echo "Available versions:"
  for ver in $HOME/haskell/ghc-*; do
    echo "  ${ver##$HOME/haskell/ghc-}"
  done
}

# Switch to a specific GHC version
ghc-switch() {
  if [ -z "$1" ]; then
    echo "USAGE: ghc-switch VERSION"
    ghc-list-available
    return 1
  fi

  VER_PATH="$HOME/haskell/ghc-$1"
  if [ -d "$VER_PATH" ]; then
    path=($VER_PATH/bin ${(@)path:#*ghc*})
    export GHC_VERSION=$1
    ghc --version
  else
    echo "GHC $1 isn't available"
    ghc-list-available
    return 1
  fi
}

# Cycle GHC versions
g() {
  case $GHC_VERSION in
    7.8.4)
      ghc-switch 7.10.2
      ;;
    7.10.2)
      ghc-switch 8.0.1
      ;;
    *)
      ghc-switch 7.8.4
      ;;
  esac
}
```

#### Bash

Add the following to your `.bashrc` or `.bash_profile`:

```
function remove-ghc-from-path() {
    # set the Internal Field Separator to be ':'
    # see http://www.tldp.org/LDP/abs/html/internalvariables.html
    IFS=:
    # convert it to an array
    t=($PATH)
    unset IFS
    # remove elements with ghc from the array
    t=(${t[@]%%*ghc*})
    IFS=:
    # set the path to the new array
    PATH="${t[*]}"
    unset IFS
}

# Switch to a specific GHC version
function ghc-switch() {
    if [ -z "$1" ]; then
        echo "USAGE: ghc-switch VERSION"
        ghc-list-available
        return 1
    fi

    VER_PATH="$HOME/haskell/ghc-$1"
    if [ -d "$VER_PATH" ]; then
        remove-ghc-from-path
        PATH=$VER_PATH/bin:$PATH
        export GHC_VERSION=$1
        ghc --version
    else
        echo "GHC $1 isn't available"
        ghc-list-available
        return 1
    fi
}

# Cycle GHC versions
function g() {
    case $GHC_VERSION in
        7.8.4)
            ghc-switch 7.10.2
            ;;
        7.10.2)
            ghc-switch 8.0.1
            ;;
        *)
            ghc-switch 7.8.4
            ;;
    esac
}
```
